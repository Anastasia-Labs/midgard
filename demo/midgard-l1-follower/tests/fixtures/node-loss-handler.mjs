// The chain handler (chain-handler.mjs) whose node goes away. The first
// sidecar serves the chain, then loses the node and writes `lostPath`. Every
// later sidecar finds the node unreachable until `backPath` exists, and then
// serves the chain again. Options: those of chain-handler.mjs, `lostPath`,
// `backPath`.
import { existsSync, writeFileSync } from "node:fs";

import chainHandler from "./chain-handler.mjs";

export default (options, controls) => {
  const chain = chainHandler(options);
  const lost = existsSync(options.lostPath);
  return {
    hello: () =>
      lost && !existsSync(options.backPath)
        ? {
            fatal: {
              code: "node_unreachable",
              message: "dial unix node.socket: connect: no such file",
              status: 69,
            },
          }
        : chain.hello(),
    openStream: (request, stream) => {
      const opened = chain.openStream(request, stream);
      if (!lost)
        setTimeout(() => {
          writeFileSync(options.lostPath, "lost");
          controls.fatal("node_connection_lost", "node socket closed", 69);
        }, 300);
      return opened;
    },
  };
};
