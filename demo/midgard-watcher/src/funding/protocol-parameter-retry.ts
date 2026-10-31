/** Retry an unsigned build once, only after a verified parameter change.
 * This function must never enclose signing, durable intent writes or submission.
 */
export const buildWithWatcherProtocolParameterRefresh = async <T>(input: {
  readonly build: () => Promise<T>;
  readonly refresh: () => Promise<boolean>;
  readonly assertCurrent: () => void;
}): Promise<T> => {
  input.assertCurrent();
  try {
    return await input.build();
  } catch (cause) {
    input.assertCurrent();
    if (!(await input.refresh())) throw cause;
    input.assertCurrent();
    return await input.build();
  }
};
