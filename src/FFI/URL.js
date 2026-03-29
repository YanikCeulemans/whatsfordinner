export const _mkImpl = (left, right, candidate) => {
  try {
    return right(new URL(candidate))
  } catch(e) {
    return left(e.message)
  }
}
export const pathname = url => url.pathname
export const search = url => url.search
