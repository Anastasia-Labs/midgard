import type { ServiceSpec } from "./supervisor.js";

/** Declarative probe binding is included in the supervisor service digest. */
export type ValidatedReadinessProbe = {
  readonly binding: string;
  readonly check: (timeoutMs: number) => Promise<boolean>;
};

export const probe = async (url: string, timeoutMs: number) => {
  try {
    const response = await fetch(url, {
      signal: AbortSignal.timeout(timeoutMs),
    });
    const body = await response.text();
    return { ok: response.ok, status: response.status, body };
  } catch (error) {
    return { ok: false, status: 0, body: String(error) };
  }
};

export const hasReadinessProbe = (service: ServiceSpec): boolean =>
  service.readyUrl !== undefined || service.readyProbe !== undefined;

export const probeServiceReadiness = async (
  service: ServiceSpec,
  timeoutMs: number,
) => {
  if (service.readyProbe === undefined)
    return service.readyUrl === undefined
      ? { ok: false, status: 0, body: "no_validated_readiness_probe" }
      : await probe(service.readyUrl, timeoutMs);
  let timer: ReturnType<typeof setTimeout> | undefined;
  try {
    const ready = await Promise.race([
      service.readyProbe.check(timeoutMs),
      new Promise<boolean>((resolve) => {
        timer = setTimeout(() => resolve(false), timeoutMs);
      }),
    ]);
    return {
      ok: ready,
      status: ready ? 200 : 503,
      body: JSON.stringify({
        ready,
        reasons: ready ? [] : ["validated_readiness_failed"],
      }),
    };
  } catch {
    return {
      ok: false,
      status: 503,
      body: JSON.stringify({
        ready: false,
        reasons: ["validated_readiness_failed"],
      }),
    };
  } finally {
    clearTimeout(timer);
  }
};
