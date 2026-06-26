export const _mkImpl = (address) => new WebSocket(address);

export const _closeImpl = (code, webSocket) => webSocket.close(code);

