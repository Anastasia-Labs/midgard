import { readFile, writeFile } from "node:fs/promises";

import {
  encodeMidgardConsensusProfile,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
} from "../dist/consensus-profile.js";

const START =
  "<!-- BEGIN MIDGARD_CONSENSUS_PROFILE_V1_GENERATED: do not edit -->";
const END = "<!-- END MIDGARD_CONSENSUS_PROFILE_V1_GENERATED -->";
const DOCUMENT_URL = new URL(
  "../../../docs/consensus-profile-v1.md",
  import.meta.url,
);

const profile = JSON.parse(
  Buffer.from(encodeMidgardConsensusProfile()).toString("utf8"),
);
const generated = [
  START,
  "",
  `Profile digest: \`${MIDGARD_CONSENSUS_PROFILE_DIGEST}\``,
  "",
  "```json",
  JSON.stringify(profile, null, 2),
  "```",
  "",
  END,
].join("\n");

const source = await readFile(DOCUMENT_URL, "utf8");
const start = source.indexOf(START);
const end = source.indexOf(END);
if (start < 0 || end < start) {
  throw new Error(
    "docs/consensus-profile-v1.md is missing the generated profile markers",
  );
}
const afterEnd = end + END.length;
const actual = source.slice(start, afterEnd);

if (process.argv.includes("--check")) {
  if (actual !== generated) {
    throw new Error(
      "docs/consensus-profile-v1.md is stale; run the profile documentation sync command",
    );
  }
} else {
  await writeFile(
    DOCUMENT_URL,
    `${source.slice(0, start)}${generated}${source.slice(afterEnd)}`,
    "utf8",
  );
}
