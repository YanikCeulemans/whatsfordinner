export const _mkImpl = (address) => new WebSocket(address);

export const _closeImpl = (webSocket) => webSocket.close();

