export const _mkImpl = (address) => new WebSocket(address);

export const _closeImpl = (webSocket) => webSocket.close();

export const _messageFromEventImpl = (candidate) => {
  if ("data" in candidate && typeof candidate.data === "string") {
    return candidate;
  }

  return null;
}
