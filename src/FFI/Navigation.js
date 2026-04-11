export const navigation = () => window.navigation;
export const _interceptImpl = (opts, evt) => {
  evt.intercept({
    handler: opts.handler,
  });
};
export const _navigateImpl = (url, n) => {
  const { finished } = n.navigate(url);
  return finished;
}
