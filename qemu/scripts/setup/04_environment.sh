#!/bin/sh

# Source common functions and variables
. /tmp/scripts/setup/common.sh

log "Starting environment setup..."

# Check required variables
check_required_vars "AM5_VERSION" "AM5_PORT_APP" "AM5_PORT_APP_PUBLIC" \
    "AM5_PORT_HTTP" "AM5_PORT_HTTP_PUBLIC" "AM5_SCRIPTS_FOLDER" \
    "AM5_VERSION_FILE" "AM5_VERSION_LATEST" "AM5_REPO" \
    "AM5_HUB_API" "AM5_MIN_VERSION"

# Set up environment script
log "Creating environment script..."
ENV_SCRIPT=/etc/profile.d/am5_env.sh

cat > ${ENV_SCRIPT} << EOF
#!/bin/bash
export AM5_NAME=accessmod
export AM5_VERSION=${AM5_VERSION}
export AM5_PORT_APP=${AM5_PORT_APP}
export AM5_PORT_APP_PUBLIC=${AM5_PORT_APP_PUBLIC}
export AM5_PORT_HTTP=${AM5_PORT_HTTP}
export AM5_PORT_HTTP_PUBLIC=${AM5_PORT_HTTP_PUBLIC}
export AM5_SCRIPTS_FOLDER=${AM5_SCRIPTS_FOLDER}
export AM5_VERSION_FILE=${AM5_VERSION_FILE}
export AM5_VERSION_LATEST=${AM5_VERSION_LATEST}
export AM5_REPO=${AM5_REPO}
export AM5_HUB_API=${AM5_HUB_API}
export AM5_MIN_VERSION=${AM5_MIN_VERSION}

# Add menu alias and auto-start
alias menu='sh ${AM5_SCRIPTS_FOLDER}/menu_init.sh'
menu
EOF

log "Setting permissions for environment script..."
chmod +x ${ENV_SCRIPT}

log "Environment setup completed"
