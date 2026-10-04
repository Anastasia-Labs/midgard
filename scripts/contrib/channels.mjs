import { relative, resolve } from "node:path";
import { matchesAny } from "../preflight/derive.mjs";
import { filesUnder, hashFiles, json } from "./files.mjs";

const registryPath =
  ".agents/skills/regenerating-goldens-and-ledgers/scripts/channels.json";
export const artifactChannels = (root) => json(resolve(root, registryPath));
export const channelIdentity = (root, channel) => {
  const registry = artifactChannels(root);
  const expand = (patterns, seen = new Set()) =>
    patterns.flatMap((pattern) => {
      if (!pattern.startsWith("@")) return [pattern];
      if (seen.has(pattern) || !registry.inputSets[pattern])
        throw new Error(`unknown/cyclic artifact input set ${pattern}`);
      return expand(registry.inputSets[pattern], new Set([...seen, pattern]));
    });
  const includes = expand([...channel.inputs, ...channel.generators]);
  const excludes = expand(channel.exclude ?? []);
  const paths = filesUnder(root)
    .map((path) => relative(root, path))
    .filter(
      (path) => matchesAny(path, includes) && !matchesAny(path, excludes),
    );
  if (!paths.length)
    throw new Error(`artifact channel ${channel.id} has no resolvable inputs`);
  return hashFiles(root, paths);
};
