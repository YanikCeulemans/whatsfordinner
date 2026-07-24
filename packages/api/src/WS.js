import { WebSocketServer } from 'ws';

export const _mkWebSocketServerImpl = (options) => new WebSocketServer(options);

export const _emitConnectionImpl = (ws, request, wss) => wss.emit('connection', ws, request);

export const _handleUpgradeImpl = (request, socket, buffer, cb, wss) => wss.handleUpgrade(request, socket, buffer, cb);

export const _sendImpl = (payload, ws) => ws.send(payload);

