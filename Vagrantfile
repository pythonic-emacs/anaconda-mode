Vagrant.configure(2) do |config|
  config.vm.box = "ubuntu/trusty32"

  config.vm.provision "shell" do |s|
    s.path = "script/deploy.sh"
    s.privileged = false
    s.keep_color = true
  end
end
