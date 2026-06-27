export const _mkImpl = (address) => new WebSocket(address);

export const _closeImpl = (code, reason, webSocket) => webSocket.close(code, reason);

