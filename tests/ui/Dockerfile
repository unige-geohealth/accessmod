FROM mcr.microsoft.com/playwright:v1.62.1-noble

WORKDIR /opt/accessmod-ui

COPY package.json package-lock.json ./
RUN npm ci --ignore-scripts

COPY tests/ui/playwright.config.js ./tests/ui/playwright.config.js
COPY tests/ui/specs ./tests/ui/specs

CMD ["./node_modules/.bin/playwright", "test", "--config=tests/ui/playwright.config.js"]
