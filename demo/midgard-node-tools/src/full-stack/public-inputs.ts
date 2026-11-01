import { isIP } from "node:net";
import { isAbsolute, resolve } from "node:path";

import { isPublicDaPeerHostname } from "midgard-watcher";

/** A production profile's public node inputs, the stack configuration's `public` section. */
export type PublicStackInputs = {
  /** Private file (0600, YAML or JSON) holding exactly the node's six secrets. */
  nodeConfigFile: string;
  /** Certificate, private key (0600) and issuing CA for the public origin. */
  tlsCertFile: string;
  tlsKeyFile: string;
  tlsCaFile: string;
  /** External Ed25519 trust roots that verify the signed release. */
  trustRootsFile: string;
  /** The unused-validator classification the reference-script deployment reads. */
  blueprintClassificationFile: string;
  /** Where the stack signs the public release; it must not exist before signing. */
  releaseDirectory: string;
  /** Host port of the optional TLS ingress; null runs the node without one. */
  ingressHostPort: number | null;
  /**
   * The public DNS name the stack's retained DA is announced at (/dns4), on
   * the DA retained-transport port; it must resolve to this host.
   */
  retainedDaHost: string;
};

const PATH_KEYS = [
  "nodeConfigFile",
  "tlsCertFile",
  "tlsKeyFile",
  "tlsCaFile",
  "trustRootsFile",
  "blueprintClassificationFile",
  "releaseDirectory",
] as const;

/** Every field of the `public` section. */
export const PUBLIC_INPUT_KEYS = [
  ...PATH_KEYS,
  "ingressHostPort",
  "retainedDaHost",
] as const;

export function parsePublicStackInputs(value: unknown): PublicStackInputs {
  if (!value || typeof value !== "object" || Array.isArray(value))
    throw new Error("public must be an object");
  const input = value as Record<string, unknown>;
  if (!Object.prototype.hasOwnProperty.call(input, "retainedDaHost"))
    throw new Error(
      "public.retainedDaHost is required: the public DNS name the stack's retained DA is announced at",
    );
  if (Object.keys(input).sort().join() !== [...PUBLIC_INPUT_KEYS].sort().join())
    throw new Error(
      `public needs exactly these fields: ${PUBLIC_INPUT_KEYS.join(", ")}`,
    );
  const paths = Object.fromEntries(
    PATH_KEYS.map((key) => {
      const path = input[key];
      if (typeof path !== "string" || !isAbsolute(path))
        throw new Error(`public.${key} must be an absolute path`);
      return [key, resolve(path)];
    }),
  ) as Record<(typeof PATH_KEYS)[number], string>;
  const port = input.ingressHostPort;
  if (
    port !== null &&
    (typeof port !== "number" ||
      !Number.isSafeInteger(port) ||
      port < 1 ||
      port > 65535)
  )
    throw new Error("public.ingressHostPort must be a TCP port or null");
  const host = input.retainedDaHost;
  // The watcher dials it as /dns4: a public name, never an address.
  if (
    typeof host !== "string" ||
    isIP(host) !== 0 ||
    !isPublicDaPeerHostname(host)
  )
    throw new Error(
      `public.retainedDaHost must be the public DNS name the stack's retained DA is announced at, not ${JSON.stringify(host)}`,
    );
  return { ...paths, ingressHostPort: port, retainedDaHost: host };
}
