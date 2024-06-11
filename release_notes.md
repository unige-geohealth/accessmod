# AccessMod Release

## How to Install/Update?

1. **Download** the appropriate full image / VM / Desktop from the list below.
2. **From AccessMod Desktop:** Go to `menu -> versions -> update`.
3. **From AccessMod VM:** Go to `menu -> Change / Update Version`.
4. **From AccessMod Docker:** Execute `docker pull fredmoser/accessmod:latest`.

More info here: [AccessMod Installation Guide](https://doc-accessmod.unepgrid.ch/display/EN/4.1.+Installation+overview)

## What Has Changed?

See the [Change Log](https://github.com/unige-geohealth/accessmod/blob/release/changes.md).

## Files

### AccessMod Desktop

- **Linux:** `accessmod-desktop-linux-amd64`
- **macOS:** `accessmod-desktop-mac-amd64`, `accessmod-desktop-mac-arm64`
- **Windows:** `accessmod-desktop-win-amd64`

### AccessMod Docker

- **Docker Image Archives:** `accessmod-docker-archive-amd64`, `accessmod-docker-archive-arm64`

### AccessMod VirtualBox

- **VirtualBox OVA:** `accessmod-virtualbox`

## Additional Information

AccessMod is Linux-based. Performance-wise, it works best without virtualization, e.g., directly from a Docker process. See an example [here](https://github.com/unige-geohealth/accessmod/wiki/Creating-and-using-AccessMod-using-docker-compose).

For Docker users, you can find the AccessMod Docker images on [Docker Hub](https://hub.docker.com/r/fredmoser/accessmod).

If you encounter any issues, please refer to our [Support Page](https://doc-accessmod.unepgrid.ch/display/EN/Support).
