export const _messageEventFromEventImpl = (candidate) => {
  if ("data" in candidate && typeof candidate.data === "string") {
    return candidate;
  }

  return null;
}
