# AccessMod Release

## What Has Changed?

See the [Change Log](https://github.com/unige-geohealth/accessmod/blob/release/changes.md).

## How to Install/Update?

### AccessMod Desktop
- Select the new version from the menu (`menu -> versions -> Remote Latest`).

### AccessMod VirtualBox
- Use the version manager and restart the server.

### AccessMod Docker
- Run `docker pull fredmoser/accessmod:latest`.

Alternatively, use the provided installer in the files listed below for your OS.

For more information, see the [AccessMod Installation Guide](https://accessmod.atlassian.net/wiki/x/XRFC).

## Files

- `accessmod-desktop-<version>-setup.exe` -> AccessMod Desktop for Windows
- `accessmod-desktop-<version>-<arm/x86>.dmg` -> AccessMod Desktop for macOS x86 and arm64/M1
- `accessmod-desktop_<version>.deb` -> AccessMod Desktop for Linux
- `accessmod-virtualbox-<version>.ova` -> AccessMod VirtualBox

## Performance

AccessMod is Linux-based. Performance-wise, it works best without virtualization, e.g., directly from a Docker process. See an example [here](https://github.com/unige-geohealth/accessmod/wiki/Creating-and-using-AccessMod-using-docker-compose).

## Issues

If you encounter any issues or want to contribute, please refer to our [issue tracker](https://github.com/unige-geohealth/accessmod/issues) or the [official documentation](https://accessmod.atlassian.net).
