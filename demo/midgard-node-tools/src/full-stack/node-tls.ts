import { readFileSync } from "node:fs";

import { Agent, setGlobalDispatcher } from "undici";

/** The variables that carry a preprod-public node's TLS trust into host commands. */
export const NODE_TLS_CA_ENV = "MIDGARD_STACK_NODE_TLS_CA_FILE";
export const NODE_TLS_SERVERNAME_ENV = "MIDGARD_STACK_NODE_TLS_SERVERNAME";

/**
 * Makes this process's fetch trust the public node's certificate authority
 * and check the certificate against the public origin's hostname, so a host
 * tool reaches the node's HTTPS port on loopback.
 */
export function trustNodeTls(caFile: string, servername: string) {
  setGlobalDispatcher(
    new Agent({ connect: { ca: readFileSync(caFile), servername } }),
  );
}
