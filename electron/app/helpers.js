import { fileURLToPath } from "node:url";
import { dirname } from "node:path";

export function getDirname(url) {
  if (!url) {
    url = import.meta.url;
  }
  const path = fileURLToPath(url);
  return dirname(path);
}

export function getAbsolutePath(path, origin) {
  if (!origin) {
    origin = import.meta.url;
  }
  const { pathname } = new URL(path, origin);
  return pathname;
}
