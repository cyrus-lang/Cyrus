%global rust_version %(CYRUS_VERSION)

Name:           cyrus
Version:        %{rust_version}
Release:        1%{?dist}

Summary:        Cyrus Programming Language

License:        GPL-3.0
URL:            https://github.com/cyrus-lang/Cyrus
Source0:        cyrus-${rust_version}.tar.gz

BuildRequires:  rust, cargo, llvm, clang, libffi-devel, libxml2-devel, isl-devel
BuildArch:	    x86_64

%description
The Cyrus Programming Language.

%prep
%setup -q

%build
cargo build --release --locked

%install

mkdir -p %{buildroot}/usr/bin
install -m 0755 target/release/cyrus %{buildroot}/usr/bin/

mkdir -p %{buildroot}/usr/share/cyrus
cp -r stdlib %{buildroot}/usr/share/cyrus/

mkdir -p %{buildroot}/etc/profile.d
echo 'export CYRUS_STDLIB_PATH=/usr/share/cyrus/stdlib' > %{buildroot}/etc/profile.d/cyrus.sh
chmod 0644 %{buildroot}/etc/profile.d/cyrus.sh

%files
/usr/bin/cyrus
/usr/share/cyrus/stdlib/*
/etc/profile.d/cyrus.sh

%post
grep -qxF 'source /etc/profile.d/cyrus.sh' /etc/bashrc || echo 'source /etc/profile.d/cyrus.sh' >> /etc/bashrc
source /etc/profile.d/cyrus.sh

%postun
sed -i '/source \/etc\/profile.d\/cyrus.sh/d' /etc/bashrc

%changelog
* Fri Sep 05 2025 Cyrus Team <you@example.com> - ${rust_version}-1
- Initial RPM packaging
