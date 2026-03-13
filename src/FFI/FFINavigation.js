export const navigation = () => window.navigation;
export const _interceptImpl = (opts, evt) => {
  evt.intercept({
    handler: opts.handler,
  });
};
