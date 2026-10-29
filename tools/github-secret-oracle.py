# Oracle for tools/github-secret.rkt: seal with the Racket code, open with pynacl.
import base64, json, subprocess, sys, os
from nacl.public import PrivateKey, SealedBox
sk = PrivateKey.generate()
pk = bytes(sk.public_key)
msg = b"packaging token oracle \x00\xff" + os.urandom(40)
here = os.path.dirname(os.path.abspath(__file__))
out = subprocess.run([sys.argv[1] if len(sys.argv) > 1 else "racket", "-e",
    f'(require (file "{here}/github-secret.rkt") net/base64) (display (base64-encode (sealed-box (base64-decode #"{base64.b64encode(pk).decode()}") (base64-decode #"{base64.b64encode(msg).decode()}")) #""))'],
    capture_output=True, text=True, check=True).stdout.strip()
opened = SealedBox(sk).decrypt(base64.b64decode(out))
print("sealed box opens with pynacl:", opened == msg)
sys.exit(0 if opened == msg else 1)
