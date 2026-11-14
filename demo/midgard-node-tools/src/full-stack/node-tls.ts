import { readFileSync } from "node:fs";

import { Agent, setGlobalDispatcher } from "undici";

/** The variables that carry a preprod-public node's TLS trust into host commands. */
export const NODE_TLS_CA_ENV = "MIDGARD_STACK_NODE_TLS_CA_FILE";
export const NODE_TLS_SERVERNAME_ENV = "MIDGARD_STACK_NODE_TLS_SERVERNAME";

/** The connection options that trust the public node's certificate authority. */
export const nodeTlsConnect = (caFile: string, servername: string) => ({
  ca: readFileSync(caFile),
  servername,
});

/**
 * Makes this process's fetch trust the public node's certificate authority
 * and check the certificate against the public origin's hostname, so a host
 * tool reaches the node's HTTPS port on loopback.
 */
export function trustNodeTls(caFile: string, servername: string) {
  setGlobalDispatcher(
    new Agent({ connect: nodeTlsConnect(caFile, servername) }),
  );
}

/**
 * The connection options the node-tls preload trusts, from its variables, or
 * undefined when they are unset (a stack whose node serves plain HTTP).
 */
export const nodeTlsConnectFromEnv = (
  env: Readonly<Record<string, string | undefined>>,
) => {
  const caFile = env[NODE_TLS_CA_ENV];
  const servername = env[NODE_TLS_SERVERNAME_ENV];
  return caFile && servername ? nodeTlsConnect(caFile, servername) : undefined;
};

/** The public origin's hostname, which the node certificate must cover. */
export const publicOriginHost = (
  env: Readonly<Record<string, string | undefined>>,
) => new URL(env.MIDGARD_PUBLIC_API_ORIGIN!).hostname;

/** The TLS a host process trusts to reach a stack's node API. */
export type StackNodeTls = Readonly<{ caFile: string; servername: string }>;

/**
 * The TLS trust a host process needs to reach a stack's node API: none for
 * a stack with no public route, whose node serves plain HTTP; for a
 * preprod-public stack, its certificate authority, checked against the
 * public origin's hostname, since the stack's loopback HTTPS ports serve the
 * public certificate.
 */
export const stackNodeTls = (
  config: Readonly<{ public?: Readonly<{ tlsCaFile: string }> }>,
  env: Readonly<Record<string, string | undefined>>,
): StackNodeTls | undefined =>
  config.public === undefined
    ? undefined
    : Object.freeze({
        caFile: config.public.tlsCaFile,
        servername: publicOriginHost(env),
      });
