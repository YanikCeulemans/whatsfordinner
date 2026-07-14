export const _mkImpl = (address) => new WebSocket(address);

export const _closeImpl = (code, reason, webSocket) => webSocket.close(code, reason);

export const _readyStateImpl = (webSocket) => webSocket.readyState;
