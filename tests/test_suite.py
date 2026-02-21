import sys
import re
import subprocess
import shlex
import shutil
import argparse
import threading
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor , as_completed
from dataclasses import dataclass
from typing import Optional , List

print_lock = threading.Lock()

@dataclass
class TestResult:
	name : str
	success : bool
	error_message : Optional[str] = None

def get_compiler_version(compiler_path : str,timeout : float = 3.0) -> Optional[str]:
	exe = shutil.which(compiler_path) or compiler_path
	try:
		cp = subprocess.run([exe,'version'],capture_output = True,text = True,timeout = timeout)
		return (cp.stdout + cp.stderr).strip() or None
	except (FileNotFoundError,subprocess.TimeoutExpired):
		return None

def extract_metadata(file_path : Path) -> dict:
	content = file_path.read_text()
	return {
		'stdout' : (re.search(r'//\s*@stdout:\s*(.*)',content) or [None,''])[1].strip(),
		'stdin' : (re.search(r'//\s*@stdin:\s*(.*)',content) or [None,''])[1].strip(),
		'args' : (re.search(r'//\s*@args:\s*(.*)',content) or [None,''])[1].strip(),
	}

def run_test(test_file : Path,base_path : Path,compiler : str,flags : str,output_dir : Path) -> TestResult:
	rel_name = str(test_file.relative_to(base_path))
	try:
		meta = extract_metadata(test_file)
		out_bin = output_dir / test_file.stem
		build_cmd = [compiler,'build',str(test_file),'-o',str(out_bin)] + shlex.split(flags)
		res = subprocess.run(build_cmd,capture_output = True,text = True)
		if res.returncode != 0:
			return TestResult(rel_name,False,f'Build Fail:\n{res.stderr}')

		run_res = subprocess.run(
			[str(out_bin)] + shlex.split(meta['args']),
			input = meta['stdin'],
			capture_output = True,
			text = True
		)

		actual = run_res.stdout.strip()
		if actual != meta['stdout']:
			return TestResult(rel_name,False,f'Expected : \'{meta['stdout']}\'\nGot : \'{actual}\'')

		return TestResult(rel_name,True)
	except Exception as e:
		return TestResult(rel_name,False,str(e))

def main():
	parser = argparse.ArgumentParser(description = 'Multi-threaded Cyrus Test Runner')
	parser.add_argument('-d','--directory',required = True)
	parser.add_argument('--compiler',default = 'cyrus')
	parser.add_argument('--flags',default = '')
	parser.add_argument('--output',required = True)
	parser.add_argument('-t','--threads',type = int,default = 4,help = 'Number of concurrent threads')

	args = parser.parse_args()
	test_files = list(Path(args.directory).rglob('*.cyrus'))
	out_path = Path(args.output)
	out_path.mkdir(parents = True,exist_ok = True)

	print(f'Running {len(test_files)} tests using {args.threads} threads ...\n')

	results = []
	with ThreadPoolExecutor(max_workers = args.threads) as executor:
		future_to_test = {
			executor.submit(run_test,f,Path(args.directory),args.compiler,args.flags,out_path) : f 
			for f in test_files
		}

		for future in as_completed(future_to_test):
			res = future.result()
			results.append(res)
			with print_lock:
				status = '[PASS]' if res.success else '[FAIL]'
				print(f'{status} {res.name}')

	failed = [r for r in results if not r.success]
	print(f'\n{'-' * 20}\nPassed : {len(results) - len(failed)} | Failed : {len(failed)}')

	for f in failed:
		print(f'\nDetails for {f.name}:\n{f.error_message}')

	if failed : sys.exit(1)

if __name__ == '__main__':
	main()
