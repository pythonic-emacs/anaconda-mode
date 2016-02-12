Vagrant.configure(2) do |config|
  config.vm.box = "ubuntu/trusty32"

  ["install_tramp", "install_emacs_build_deps", "deploy"].each do |script|
    config.vm.provision "shell" do |shell|
      shell.path = "script/#{script}.sh"
      shell.privileged = false
      shell.keep_color = true
    end
  end
end
