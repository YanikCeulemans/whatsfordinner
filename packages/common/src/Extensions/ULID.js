// We implement pretty much the same implementation from the library's window
// module because `globalThis` exists and it intended to be a cross platform
// way to interact with API's that are shared between node and the browser.
export const prng = () => {
  const buf = new UInt32Array(1);
  globalThis.crypto.getRandomValues(buf);
  // divide the generated 32 bit uint by the max 32 bit uint value + 1 to
  // obtain a value between 0 inclusive and 1 exclusive, which is what prng is
  // supposed to produce
  return buf[0] / 2 ** 32;
}
