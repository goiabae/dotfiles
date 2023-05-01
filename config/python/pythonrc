import os
import atexit
import readline

if 'PYTHONHISTFILE' in os.environ:
  history = os.path.expanduser(os.environ['PYTHONHISTFILE'])
elif 'XDG_CACHE_HOME' in os.environ:
  history = os.path.join(os.path.expanduser(os.environ['XDG_CACHE_HOME']), 'python', 'python_history')
else:
  history = os.path.join(os.path.expanduser('~'), '.python_history')

history = os.path.abspath(history)
_dir, _ = os.path.split(history)
os.makedirs(_dir, exist_ok=True)

try:
  readline.read_history_file(history)
except OSError:
  pass

def write_history():
  try:
    readline.write_history_file(history)
  except OSError:
    pass

atexit.register(write_history)
