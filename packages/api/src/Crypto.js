import crypto from 'node:crypto';

export const _sha1Impl = (encoding, input) => crypto.hash('sha1', input, { outputEncoding: encoding });
