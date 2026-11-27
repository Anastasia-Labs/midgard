import { join } from "node:path";
import { pathToFileURL } from "node:url";

import { SUBMIT_HTTP_BODY_MAX_BYTES } from "midgard-node/commands/http/submission-capacity";
import { CLIENT_ACTIVE_MAX } from "midgard-node/commands/listen-http-rate-limit";

type PublicIngressModule = {
  renderPublicIngress: (
    env: Record<string, string | undefined>,
    options: {
      socketPath: string;
      submitBodyBytes: number;
      clientActiveMax: number;
    },
  ) => string;
};

/**
 * The public ingress's nginx configuration, rendered by the node's own
 * renderer (`scripts/public-ingress.mjs` under `nodeRoot`) with the node's
 * own limits: its largest submit body and its per-client cap on requests in
 * flight, so the ingress never admits what the node would refuse.
 */
export const renderNodePublicIngress = async (
  nodeRoot: string,
  env: Record<string, string | undefined>,
  socketPath: string,
): Promise<string> => {
  const { renderPublicIngress } = (await import(
    pathToFileURL(join(nodeRoot, "scripts/public-ingress.mjs")).href
  )) as PublicIngressModule;
  return renderPublicIngress(env, {
    socketPath,
    submitBodyBytes: SUBMIT_HTTP_BODY_MAX_BYTES,
    clientActiveMax: CLIENT_ACTIVE_MAX,
  });
};
