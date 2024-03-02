# Version Manager

[ early version. Using in prod not recommanded ]


## Description
The Version Manager is a tool designed to manage software versioning using semantic versioning principles (semver). It automates the process of creating version messages, updating changelogs, and maintaining a clean versioning history. The tool integrates with Git to commit and tag versions, ensuring that version management is seamlessly integrated with source control.

## Features
- Utilizes semver for versioning
- Creates and edits version messages
- Automatically updates changelogs
- Performs dry runs to preview changes without applying them
- Commits and tags versions in Git
- Pushes changes to remote repositories

## Usage

By default, expects `version.txt` file and `changes.md` changelog file. Configurable.

In your package.json 

```json
{
  scripts :{
    version : 'node @fxi/version_manager'
}
}

```

From the git repo for which create the version : 

```sh 
npm run version
```

## Developer

### Available Scripts
- `test`: Run tests using Vitest.
- `test-debug`: Run tests in debug mode with Vitest.



## License
This project is licensed under the MIT License - see the LICENSE file for details.
