cask:
  git.latest:
    - name: https://github.com/cask/cask
    - target: /usr/share/cask
  cmd.run:
    - name: ln -s /usr/share/cask/bin/cask /usr/sbin/cask

melpa:
  cmd.run:
    - name: cask install
    - cwd: {{ salt['pillar.get']('build_dir') }}
