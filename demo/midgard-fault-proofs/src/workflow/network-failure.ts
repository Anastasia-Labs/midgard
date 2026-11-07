/** A socket-level fetch failure (reset, refused, timed out): the peer said nothing. */
export const isNetworkFailure = (error: unknown): boolean => {
  const cause =
    error instanceof TypeError && "cause" in error ? error.cause : error;
  return (
    cause instanceof Error &&
    "code" in cause &&
    typeof cause.code === "string" &&
    [
      "UND_ERR_SOCKET",
      "UND_ERR_CONNECT_TIMEOUT",
      "UND_ERR_HEADERS_TIMEOUT",
      "UND_ERR_BODY_TIMEOUT",
      "ECONNRESET",
      "ECONNREFUSED",
      "ECONNABORTED",
      "EPIPE",
      "ETIMEDOUT",
      "EAI_AGAIN",
      "ENETUNREACH",
      "EHOSTUNREACH",
    ].includes(cause.code)
  );
};
