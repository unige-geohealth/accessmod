import fetch from "node-fetch";

const cache = {};

/**
 * Fetches data from a URL with caching capability.
 * If cached data is available and not expired, it returns the cached data.
 * Otherwise, it fetches from the network, caches the new data, and returns it.
 *
 * @param {string} url - The URL to fetch the data from.
 * @param {number} ttl - Time to live for the cache in seconds.
 * @returns {Promise<any>} The fetched data.
 */
export async function fetchCacheData(url, ttl) {
  const now = Date.now();
  const item = cache[url];

  // Check if item exists and is still valid
  if (item && item.expires > now) {
    return item.data;
  }

  // If the item is expired or doesn't exist, fetch new data
  const res = await fetch(url);
  if (!res.ok) {
    throw new Error(
      `HTTP error while fetching data. Status: ${res.status} - ${res.statusText}`
    );
  }

  const data = await res.json();
  const expires = now + ttl * 1000; // Calculate the expiration time

  // Cache the new data with expiration time
  cache[url] = { data, expires };

  return data;
}
