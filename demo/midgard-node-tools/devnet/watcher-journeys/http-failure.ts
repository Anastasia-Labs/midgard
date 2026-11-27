/** How much of a refused answer's body an error carries. */
const BODY_EXCERPT_CHARACTERS = 512;

/**
 * What a non-2xx answer says, as a suffix for its error: a service that
 * refuses a read names why in its body (a watcher's 503 names its journal
 * fault or that it is starting), and an error with only the status names
 * nothing to act on. Empty when the body is empty or cannot be read.
 */
export const httpFailureDetail = async (response: Response) => {
  const body = (await response.text().catch(() => "")).trim();
  return body.length === 0 ? "" : `: ${body.slice(0, BODY_EXCERPT_CHARACTERS)}`;
};
