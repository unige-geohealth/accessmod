import { defineConfig, externalizeDepsPlugin } from 'electron-vite'

export default defineConfig({
  main: {
    plugins: [externalizeDepsPlugin()],
  },
  preload: {
    plugins: [externalizeDepsPlugin()],
    build: {
      lib : {
        entry : './src/preload/index.js',
        formats : 'es'
      },
    },
  },
  renderer: {}
})
