packer {
  required_plugins {
    virtualbox = {
      version = "~> 1"
      source  = "github.com/hashicorp/virtualbox"
    }
  }
}

variable "cpus" {
  type    = string
  default = "2"
}

variable "disk_size" {
  type    = string
  default = "40960"
}

variable "hub_api" {
  type    = string
  default = "https://hub.docker.com/v2/"
}


# used in hub.docker to filter versions that starts with x
variable "minversion" {
  type    = string
  default = "5.8"
}

# version of the vm : should cover all 5.8.* version
variable "vm_version" {
  type    = string
  default = "5.8"
}

variable "repo" {
  type    = string
  default = "fredmoser/accessmod"
}

variable "version" {
  type    = string
  default = "5.8.1"
}

variable "iso_checksum" {
  type    = string
  default = "160c76f65ef888cb4cb4758a3b9d81d44f597ce22631aa94ae164c6a529d7b43"
}

variable "iso_url" {
  type    = string
  default = "https://dl-cdn.alpinelinux.org/alpine/v3.16/releases/x86_64/alpine-virt-3.16.8-x86_64.iso"
}

variable "alpine_repo" {
  type = string
  default = "http://dl-cdn.alpinelinux.org/alpine/v3.16/community"
}

variable "memory" {
  type    = string
  default = "4096"
}

variable "port_app" {
  type    = string
  default = "3000"
}

variable "port_app_public" {
  type    = string
  default = "8080"
}

variable "port_http" {
  type    = string
  default = "5000"
}

variable "port_http_public" {
  type    = string
  default = "8888"
}

variable "port_ssh" {
  type    = string
  default = "22"
}

variable "port_ssh_public" {
  type    = string
  default = "2222"
}

variable "vm_name" {
  type    = string
  default = "alpine-accessmod"
}

variable "ssh_password" {
  type    = string
  default = "L3b5SqNDIoarhn5CJJUJvA=="
}

variable "ssh_username" {
  type    = string
  default = "root"
}

variable "username" {
  type = string
  default = "accessmod"
}
variable "userpassword" {
  type = string
  default = "accessmod"
}


source "virtualbox-iso" "accessmod" {
  iso_url      = "https://dl-cdn.alpinelinux.org/alpine/v3.16/releases/x86_64/alpine-virt-3.16.8-x86_64.iso"
  iso_checksum = "160c76f65ef888cb4cb4758a3b9d81d44f597ce22631aa94ae164c6a529d7b43"

  boot_command = [
    "${var.ssh_username}<enter><wait>",
    "setup-alpine<enter><wait10>",
    "us us<enter><wait>",
    "accessmod<enter><wait>",
    "eth0<enter><wait>",
    "dhcp<enter><wait>",
    "n<enter><wait10>",
    "${var.ssh_password}<enter>",
    "${var.ssh_password}<enter><wait5>",
    "UTC<enter><wait>",
    "none<enter><wait>",
    "chrony<enter><wait10>",
    "1<enter><wait>",
    "no<enter><wait>",
    "openssh<enter><wait5>", 
    "yes<enter>",
    "none<enter>",
    "sda<enter><wait>",
    "sys<enter><wait>",
    "y<enter><wait10>",
    "mount /dev/sda3 /mnt<enter><wait>",
    "echo 'PermitRootLogin yes' >> /mnt/etc/ssh/sshd_config<enter>",
    #"chmod 700 /mnt/root/.ssh",
    #"chmod 600 /mnt/root/.ssh/authorized_keys",
    "umount /mnt<enter><wait>",
    "reboot<enter><wait>",
  ]
  vm_name = "${var.vm_name}-${var.vm_version}"
  boot_wait            = "10s"
  disk_size            = "${var.disk_size}"
  guest_os_type        = "Linux_64"
  http_directory       = "http"
  shutdown_command     = "poweroff -d 1"
  ssh_username         = "${var.ssh_username}"
  ssh_password         = "${var.ssh_password}"
  ssh_timeout          = "20m"
  ssh_handshake_attempts = "20"
  ssh_port             = "${var.port_ssh_public}"
  format               = "ova"
  headless             = false
  vrdp_bind_address    = "127.0.0.1"
  skip_nat_mapping     = true
  output_directory     = "_build"
  nic_type 	       = "virtio"  
  vboxmanage = [
    ["modifyvm", "{{ .Name }}", "--memory", "${var.memory}"],
    ["modifyvm", "{{ .Name }}", "--cpus", "${var.cpus}"],
    ["modifyvm", "{{ .Name }}", "--rtcuseutc", "on"],
    ["modifyvm", "{{ .Name }}", "--graphicscontroller", "VMSVGA"],
    ["modifyvm", "{{ .Name }}", "--vram", "9"],
    ["modifyvm", "{{ .Name }}", "--vrde", "off"],
    ["modifyvm", "{{ .Name }}", "--nic1", "nat"],
    ["modifyvm", "{{ .Name }}", "--nat-localhostreachable1", "on"],
    ["modifyvm", "{{ .Name }}", "--natpf1", "accessmod_app,tcp,0.0.0.0,${var.port_app_public},0.0.0.0,${var.port_app}"],
    ["modifyvm", "{{ .Name }}", "--natpf1", "accessmod_http,tcp,0.0.0.0,${var.port_http_public},0.0.0.0,${var.port_http}"],
    ["modifyvm", "{{ .Name }}", "--natpf1", "accessmod_ssh,tcp,0.0.0.0,${var.port_ssh_public},0.0.0.0,${var.port_ssh}"],
    ["modifyvm", "{{ .Name }}", "--nictype1", "virtio"],
    ["modifyvm", "{{ .Name }}", "--nictype2", "virtio"],
    ["modifyvm", "{{ .Name }}", "--nictype3", "virtio"],
    ["modifyvm", "{{ .Name }}", "--chipset", "ich9"],
    ["modifyvm", "{{ .Name }}", "--ioapic", "on"],
    ["modifyvm", "{{ .Name }}", "--usb", "off"],
    ["modifyvm", "{{ .Name }}", "--usbehci", "off"],
    ["modifyvm", "{{ .Name }}", "--mouse", "ps2"],
    ["modifyvm", "{{ .Name }}", "--audio", "none"]
  ]

}


build {
  description = "Provsion AccessMod x86_64"
  sources     = ["source.virtualbox-iso.accessmod"]

  provisioner "file" {
    source      = "scripts"
    destination = "/tmp/scripts"
  }

  provisioner "shell" {
    script = "scripts/provision.sh"
    environment_vars = [
      "USERNAME=${var.username}",
      "PASSWORD=${var.userpassword}",
      "ALPINE_REPOSITORY=${var.alpine_repo}",
      "AM5_PORT_APP=${var.port_app}",
      "AM5_PORT_APP_PUBLIC=${var.port_app_public}",
      "AM5_PORT_HTTP=${var.port_http}",
      "AM5_PORT_HTTP_PUBLIC=${var.port_http_public}",
      "AM5_SCRIPTS_FOLDER=/home/accessmod/scripts",
      "AM5_VERSION_FILE=/home/accessmod/version",
      "AM5_VERSION=${var.version}",
      "AM5_MIN_VERSION=${var.minversion}",      
      "AM5_REPO=${var.repo}",
      "AM5_HUB_API=${var.hub_api}",
    ]
  }
}
