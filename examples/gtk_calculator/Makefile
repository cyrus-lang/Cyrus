debug:
	../../target/debug/cyrus run ./main.cyrus  \
		--linker-flags="-lgtk-3" \
		--linker-flags="-lgobject-2.0" \
		--linker-flags="-lglib-2.0" \
		--optimize=o0 --sanitize=address -g

release:
	../../target/debug/cyrus run ./main.cyrus  \
		--linker-flags="-lgtk-3" \
		--linker-flags="-lgobject-2.0" \
		--linker-flags="-lglib-2.0" \
		--optimize=o3