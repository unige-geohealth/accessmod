# AccessMod QEMU VM Builder

This directory contains scripts to build an AccessMod VM image using QEMU and alpine-make-vm-image. This approach replaces the previous VirtualBox-based Packer build to improve CI compatibility by eliminating nested virtualization requirements.

## Requirements

- Alpine Linux host system (or container)
- `alpine-make-vm-image` package installed
- QEMU tools installed

## Configuration

All build settings are configured in `config.sh`, including:
- System resources (CPU, memory, disk size)
- Network ports
- User credentials
- Docker image settings
- Build paths

## Building the VM

1. Make the build script executable:
```bash
chmod +x build.sh
```

2. Run the build script:
```bash
./build.sh
```

The script will:
1. Create a VMDK image using alpine-make-vm-image
2. Configure the system (users, SSH, services)
3. Install and configure Docker
4. Set up AccessMod environment
5. Output the final image to `_build/vm/alpine-accessmod-5.8.vmdk`

## Output

The build process creates a VMDK file that can be used with:
- VirtualBox
- VMware
- Other virtualization platforms that support VMDK format

## Network Ports

The VM is configured with the following port mappings:
- SSH: 22 -> 2222
- AccessMod App: 3000 -> 8080
- HTTP: 5000 -> 8888
