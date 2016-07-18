# FROM haskell:8.0.1
# ADD . /brainfuck

cd /brainfuck \
&& if [ -e "stack" ]; then \
  rm -rf /root/.stack; \
  ln -s /brainfuck/stack /root/.stack; \
fi \
&& stack install \
&& cp /root/.local/bin/bf-exe /main \
&& du -sh /main
