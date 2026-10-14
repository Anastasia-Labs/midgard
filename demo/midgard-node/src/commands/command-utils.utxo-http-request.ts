export class UtxoReadDataLimitError extends Error {
  constructor() {
    super("Midgard node UTxO data exceeds public read capacity");
    this.name = "UtxoReadDataLimitError";
  }
}

const boundedUtxoJson = async (response: Response): Promise<unknown> => {
  const reader = response.body?.getReader();
  if (reader === undefined)
    throw new Error("Midgard node UTxO page has no body");
  const chunks: Uint8Array[] = [];
  let bytes = 0;
  try {
    while (true) {
      const chunk = await reader.read();
      if (chunk.done) break;
      bytes += chunk.value.byteLength;
      if (bytes > 1_048_576)
        throw new Error("Midgard node UTxO page exceeds the response limit");
      chunks.push(chunk.value);
    }
    return JSON.parse(Buffer.concat(chunks).toString("utf8")) as unknown;
  } finally {
    await reader.cancel();
    reader.releaseLock();
  }
};
export const readUtxoPage = async (
  url: string,
  init: RequestInit,
  deadline: number,
  timeoutMs: number,
): Promise<unknown> => {
  for (let attempt = 0; attempt < 3; attempt += 1) {
    const remaining = deadline - Date.now();
    if (remaining <= 0)
      throw new Error("Midgard node UTxO query exceeded its overall deadline");
    const response = await fetch(url, {
      ...init,
      signal: AbortSignal.timeout(Math.max(1, Math.min(timeoutMs, remaining))),
    });
    if (response.ok) return boundedUtxoJson(response);
    const retry = response.headers.get("retry-after");
    const failure = await boundedUtxoJson(response).catch(() => undefined);
    if (
      response.status === 503 &&
      typeof failure === "object" &&
      failure !== null &&
      "error_code" in failure &&
      failure.error_code === "E_HTTP_READ_DATA_LIMIT"
    )
      throw new UtxoReadDataLimitError();
    if (
      attempt === 2 ||
      ![429, 503].includes(response.status) ||
      retry === null ||
      !/^[1-5]$/u.test(retry) ||
      Number(retry) * 1_000 >= deadline - Date.now()
    )
      throw new Error(
        `Midgard node UTxO query failed (${response.status.toString()})`,
      );
    await new Promise<void>((resolve) =>
      setTimeout(resolve, Number(retry) * 1_000),
    );
  }
  throw new Error("Midgard node UTxO query retries exhausted");
};
export const paceUtxoPage = () =>
  new Promise<void>((resolve) => setTimeout(resolve, 120));

/** Sequential splitting keeps at most 100 initial chunks plus seven split levels. */
export async function* readUtxoBatchPages(
  url: string,
  outRefs: readonly string[],
  deadline: number,
) {
  const pending: (readonly string[])[] = [];
  for (let offset = 0; offset < outRefs.length; offset += 100)
    pending.push(outRefs.slice(offset, offset + 100));
  pending.reverse();
  let first = true;
  while (pending.length > 0) {
    const batch = pending.pop()!;
    if (!first) await paceUtxoPage();
    first = false;
    try {
      const page = await readUtxoPage(
        url,
        {
          method: "POST",
          headers: { "content-type": "application/json" },
          body: JSON.stringify(batch),
        },
        deadline,
        10_000,
      );
      yield { page, requestedCount: batch.length };
    } catch (error) {
      if (!(error instanceof UtxoReadDataLimitError) || batch.length === 1)
        throw error;
      const middle = Math.floor(batch.length / 2);
      pending.push(batch.slice(middle), batch.slice(0, middle));
    }
  }
}
