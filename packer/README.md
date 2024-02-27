# Packer: Build and Provision Base VM Version of AccessMod

## Build Commands

- Build: `cd packer/; ./build.sh`
- Sync scripts with VM (debug): `./scripts/sync_scripts_vm.sh`
- SSH to VM: `ssh -p 2222 accessmod@localhost`
- Password: `accessmod`

## Security

This build is considered insecure. For a more secure version:
- Build using SSH keys instead of passwords.
- Disable root SSH login.
- Disable SSH password authentication.
- Disable `"%wheel ALL=(ALL) NOPASSWD: ALL"` for `/etc/sudoers.d/wheel`

## Note

The Alpine system for unattended installs (using an answer file) still requires interaction, e.g., for entering the root password and such. The current method simply follows the `setup-alpine` sequence, but it's not robust. A more reliable way to set up the machine should be found.

