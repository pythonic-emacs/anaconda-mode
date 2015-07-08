Vagrant.configure(2) do |config|
  config.vm.box = "ubuntu/trusty32"

  config.vm.provision "ansible" do |ansible|
    ansible.playbook = "playbook.yml"
  end
end
