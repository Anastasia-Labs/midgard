/**
 * The global `--l1 node|kupmios|blockfrost` option of the node and tooling
 * binaries: it selects a tool's L1 access (`l1-command-access.ts`) by
 * setting `L1_ACCESS` before the command runs. A role command (`listen`)
 * refuses any such selection at start: a role reads L1 only through its
 * follower.
 */
import { type Command, Option } from "commander";

import { TOOL_L1_ACCESS_KINDS } from "../l1-access.js";

export const registerL1AccessOption = (program: Command): Command =>
  program
    .addOption(
      new Option(
        "--l1 <access>",
        "The L1 access a command reads and submits through (default node when L1_NODE_SOCKET_PATH is set; env L1_ACCESS)",
      ).choices(TOOL_L1_ACCESS_KINDS),
    )
    .hook("preAction", (root) => {
      const selected = root.opts<{ l1?: string }>().l1;
      if (selected !== undefined) process.env.L1_ACCESS = selected;
    });
