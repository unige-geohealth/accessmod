# AccessMod Desktop

## Overview

AccessMod Desktop is an Electron-based application. This README provides instructions on how to set up a development environment and build the application for different platforms.

## Main Commands

Execute the following commands from the terminal at the root of the project directory:

- **Development Mode**:

```bash
yarn dev
```

- **Lint Code**:

```bash
yarn lint
```

- **Build Applications**:

```bash
yarn build
yarn build:mac
yarn build:linux
yarn build:win
```

## Development Setup

During a development session, manual connection to the debugger is required:

1. Open Google Chrome.
2. Navigate to `chrome://inspect/#devices`.
3. Click on "Configure..." if your device is not listed.
4. Under "Remote Target", find and select your Electron instance.
5. Launch the application.

## Building for Production

The build process supports multiple architectures and platforms and is automated using GitHub Actions:

- **Note**: It is not recommended to perform local builds for production. The automated process ensures the correct settings and tools are used.

## Important Notes

- **Product Naming**: The product name must not contain spaces (e.g., `AccessMod-Desktop`). The Debian installer (.deb) will fail otherwise.
- **Unsupported Packages**: Due to sandboxing limitations, Snap, Flatpak, and AppImage are not supported as they cannot communicate with the Docker socket.
- **Mac Builds**: Building for macOS requires code signing and notarization. Currently, only signing is implemented.
