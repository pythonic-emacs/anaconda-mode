Vagrant.configure(2) do |config|
  config.vm.box = "ubuntu/trusty32"

  config.vm.provision "ansible" do |ansible|
    ansible.playbook = "deploy/playbook.yml"
    ansible.groups = {
      "vagrant" => ["default"],
    }
  end
end
