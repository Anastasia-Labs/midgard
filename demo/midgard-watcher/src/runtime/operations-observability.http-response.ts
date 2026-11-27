export const jsonResponse = (statusCode: number, value: unknown): Response =>
  new Response(JSON.stringify(value), {
    status: statusCode,
    headers: Object.freeze({
      "cache-control": "no-store",
      "content-type": "application/json; charset=utf-8",
      "x-content-type-options": "nosniff",
    }),
  });
