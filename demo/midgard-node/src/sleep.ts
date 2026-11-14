/**
 * Resolves after `milliseconds`, the plain-promise delay used by the node's
 * non-Effect command and supervisor paths.
 */
export const sleep = (milliseconds: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, milliseconds));
