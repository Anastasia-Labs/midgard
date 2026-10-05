import type { WatcherRollbackDurableTrustedHead } from "../l1/rollback-engine.js";
import { LOOPBACK_HOSTS } from "./trusted-head-authority.exact-record.js";
import { openSelectedAuthorityStore } from "./trusted-head-authority.selected-store.js";

/** Operational runtime opens only an already selected authenticated backend. */
export const openWatcherTrustedHeadAuthorityStore = openSelectedAuthorityStore;

export type WatcherTrustedHeadAuthorityClient = Readonly<{
  readRecordAuthenticationKeyId(): Promise<string>;
  readCurrent(): Promise<WatcherRollbackDurableTrustedHead | null>;
  compareAndSwap(input: {
    readonly expectedTrustedHead: WatcherRollbackDurableTrustedHead | null;
    readonly nextTrustedHead: WatcherRollbackDurableTrustedHead;
  }): Promise<boolean>;
}>;

export const endpointUrl = (value: unknown): URL => {
  let url: URL;
  try {
    url = new URL(String(value));
  } catch {
    throw new Error("trusted-head authority endpoint is invalid");
  }
  if (
    url.protocol !== "http:" ||
    !LOOPBACK_HOSTS.has(url.hostname.toLowerCase()) ||
    url.username !== "" ||
    url.password !== "" ||
    url.search !== "" ||
    url.hash !== "" ||
    (url.pathname !== "/" && url.pathname !== "")
  ) {
    throw new Error("trusted-head authority endpoint must be loopback HTTP");
  }
  return url;
};
