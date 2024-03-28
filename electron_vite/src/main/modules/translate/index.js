import dict from "./json/dict.json" assert { type: "json" };

export function tl(id, lang = "en", obj = {}, def = "") {
  const hasTemplate = Object.keys(obj).length;
  const itemData = dict.find((d) => d.id === id) || {};

  let message = itemData[lang] || itemData["en"] || def;

  const isString = typeof message === "string";

  if (isString && hasTemplate) {
    for (const key in obj) {
      const regex = new RegExp(`{{${key}}}`, "g");
      message = message.replace(regex, obj[key]);
    }
  }

  if (!message) {
    message = id;
  }

  return message;
}
