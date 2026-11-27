/**
 * The tools CLI's typed environment edge.
 *
 * Commands in this package take the environment as an injected `env`
 * parameter defaulting to `process.env`, so their parsing is testable without
 * mutating the real process. The reads the command wiring in `src/index.ts`
 * needs live here, in the same injectable shape.
 */

export type L1KupmiosEnvironment = {
  readonly providerFailover: string | undefined;
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
};

/**
 * The local state-correction authority's Kupo and Ogmios endpoints and its
 * failover switch, read exactly as it consumes them: the failover stays an
 * optional string because the consumer normalizes it itself (it treats blank
 * and `"false"` as off). The endpoints are the local Compose stack's loopback
 * host ports (`KUPO_PORT`, `OGMIOS_PORT`, defaulting as the Compose file
 * publishes them); the node itself reads neither.
 */
export const l1KupmiosEnvironment = (
  env: NodeJS.ProcessEnv = process.env,
): L1KupmiosEnvironment => ({
  providerFailover: env.L1_PROVIDER_FAILOVER,
  kupoUrl: `http://127.0.0.1:${env.KUPO_PORT ?? "1442"}`,
  ogmiosUrl: `http://127.0.0.1:${env.OGMIOS_PORT ?? "1337"}`,
});

/** L1_PROVIDER_FAILOVER is off when unset, blank or "false" in any case. */
export const l1ProviderFailoverEnabled = (value: string | undefined) =>
  value !== undefined &&
  value.trim() !== "" &&
  value.trim().toLowerCase() !== "false";

export type StressNetwork = "Mainnet" | "Preprod";

/**
 * The network the L2 throughput stress run reports itself as targeting.
 *
 * Only `Mainnet` is recognized; every other value, including `Preview` and
 * `Custom`, reads as `Preprod`. That is the behaviour this had when it was
 * inlined, and it is preserved here rather than tightened, because the value
 * only labels a stress summary. It is stated explicitly so the narrowing is a
 * decision on the page instead of a silent `?:` in the middle of an options
 * object.
 */
export const stressNetworkFromEnvironment = (
  env: NodeJS.ProcessEnv = process.env,
): StressNetwork => (env.NETWORK === "Mainnet" ? "Mainnet" : "Preprod");
