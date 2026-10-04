import { randomUUID } from "node:crypto";
import {
  existsSync,
  lstatSync,
  mkdirSync,
  readFileSync,
  readdirSync,
  realpathSync,
  rmSync,
  readlinkSync,
  openSync,
  closeSync,
  unlinkSync,
  rmdirSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { resolve } from "node:path";
import { setTimeout as delay } from "node:timers/promises";

import { atomicJson, json, sha256 } from "./files.mjs";

export const resourceDirectory = () => {
  const directory = resolve(
    tmpdir(),
    `midgard-contrib-${process.getuid?.() ?? "local"}`,
  );
  mkdirSync(directory, { mode: 0o700, recursive: true });
  const stats = lstatSync(directory);
  if (
    !stats.isDirectory() ||
    stats.isSymbolicLink() ||
    (stats.mode & 0o077) !== 0 ||
    (process.getuid && stats.uid !== process.getuid())
  ) {
    throw new Error(`unsafe resource registry permissions/owner: ${directory}`);
  }
  return directory;
};

export const processIdentity = (pid = process.pid) => {
  try {
    // Linux stat comm may contain spaces or parentheses; fields start after
    // the LAST ')'. starttime is field 22, ppid field 4.
    const stat = readFileSync(`/proc/${pid}/stat`, "utf8");
    const fields = stat.slice(stat.lastIndexOf(")") + 2).split(" ");
    return {
      pid,
      start: fields[19],
      parent: Number(fields[1]),
      boot: readFileSync("/proc/sys/kernel/random/boot_id", "utf8").trim(),
      namespace: readlinkSync(`/proc/${pid}/ns/pid`),
      state: fields[0],
      group: Number(fields[2]),
    };
  } catch (error) {
    if (error.code === "ENOENT") return undefined;
    throw new Error(`cannot attest process ${pid}: ${error.message}`);
  }
};

export const sameProcess = (owner) => {
  if (!owner) return false;
  const current = processIdentity(owner.pid);
  return (
    current !== undefined &&
    current.start === owner.start &&
    current.boot === owner.boot &&
    (!owner.namespace || current.namespace === owner.namespace)
  );
};

const isDescendant = (owner) => {
  let current = processIdentity();
  const visited = new Set();
  while (current && !visited.has(current.pid)) {
    if (current.pid === owner.pid) return sameProcess(owner);
    visited.add(current.pid);
    current = current.parent > 0 ? processIdentity(current.parent) : undefined;
  }
  return false;
};

const groupsState = (directory, ownerToken) => {
  const path = resolve(directory, "processes");
  if (!existsSync(path)) return "abandoned";
  const self = processIdentity();
  const groups = readdirSync(path).map((name) => {
    if (!name.endsWith(".json"))
      throw new Error("incomplete child process record");
    return json(resolve(path, name));
  });
  for (const group of groups) {
    if (
      group.schema !== "midgard-child-group/v1" ||
      group.ownerToken !== ownerToken ||
      group.phase !== "running" ||
      !Number.isSafeInteger(group.pid) ||
      group.pid <= 0 ||
      group.namespace !== self.namespace ||
      !/^[a-f0-9-]{36}$/u.test(group.boot ?? "")
    )
      return "unknown";
    if (group.boot !== self.boot) continue;
    for (const pid of readdirSync("/proc").filter((name) =>
      /^\d+$/u.test(name),
    )) {
      try {
        const stat = readFileSync(`/proc/${pid}/stat`, "utf8");
        const fields = stat.slice(stat.lastIndexOf(")") + 2).split(" ");
        if (Number(fields[2]) === group.pid && fields[0] !== "Z")
          return "owned";
      } catch (error) {
        if (error.code !== "ENOENT") return "unknown";
      }
    }
  }
  return "abandoned";
};

const ownerState = (record, directory) => {
  const owner = record.owner;
  if (
    !owner ||
    !Number.isSafeInteger(owner.pid) ||
    owner.pid <= 0 ||
    !/^\d+$/u.test(owner.start ?? "") ||
    !/^[a-f0-9-]{36}$/u.test(owner.boot ?? "")
  )
    return "unknown";
  const self = processIdentity();
  if (owner.boot !== self.boot) return "abandoned";
  if (owner.namespace && owner.namespace !== self.namespace) return "unknown";
  const current = processIdentity(owner.pid);
  if (sameProcess(owner) && current.state !== "Z") return "owned";
  if (!owner.namespace || record.tracksProcessGroups !== true) return "unknown";
  return groupsState(directory, record.token);
};

// Record launches before spawn, closing the owner-death gap. An unfinished
// launch or an invisible PID namespace is unknown and cannot be reclaimed.
export const registerOwnedLaunch = (env) => {
  const tokens = env.MIDGARD_CONTRIB_RESOURCE_TOKENS?.split(",") ?? [];
  const records = [];
  const self = processIdentity();
  try {
    for (const name of readdirSync(resourceDirectory()).filter((name) =>
      name.endsWith(".lease"),
    )) {
      const directory = resolve(resourceDirectory(), name);
      let owner;
      try {
        owner = json(resolve(directory, "owner.json"));
      } catch (error) {
        if (error.code === "ENOENT") continue;
        throw error;
      }
      if (!tokens.includes(owner.token)) continue;
      if (!isDescendant(owner.owner))
        throw new Error("cannot attest inherited launch ownership");
      const path = resolve(directory, "processes", `${randomUUID()}.json`);
      const record = {
        schema: "midgard-child-group/v1",
        phase: "pending",
        ownerToken: owner.token,
        boot: self.boot,
        namespace: self.namespace,
      };
      atomicJson(path, record);
      records.push({ path, record });
    }
  } catch (error) {
    for (const { path } of records) unlinkSync(path);
    throw error;
  }
  return {
    attach: (pid) => {
      for (const entry of records) {
        entry.record = {
          ...entry.record,
          phase: "running",
          pid,
          start: processIdentity(pid)?.start,
        };
        atomicJson(entry.path, entry.record);
      }
    },
    joined: () => {
      for (const entry of records) {
        if (json(entry.path).ownerToken !== entry.record.ownerToken)
          throw new Error("child process record ownership changed");
        unlinkSync(entry.path);
      }
    },
  };
};

export const listResources = () =>
  readdirSync(resourceDirectory())
    .filter((name) => name.endsWith(".lease"))
    .map((name) => {
      const path = resolve(resourceDirectory(), name);
      try {
        const record = json(resolve(path, "owner.json"));
        return {
          ...record,
          path,
          state: ownerState(record, path),
        };
      } catch (error) {
        return { path, state: "unknown", reason: error.message };
      }
    });

export const withResource = async (
  resource,
  action,
  // A cold recursive workspace build can legitimately queue behind several
  // admitted compilers. Keep admission bounded by the process deadline, not
  // a two-minute timeout that aborts otherwise healthy parallel pnpm builds.
  { signal, timeoutMs = 1_800_000, env = process.env } = {},
) => {
  const directory = resolve(resourceDirectory(), `${sha256(resource)}.lease`);
  const inherited = env.MIDGARD_CONTRIB_RESOURCE_TOKENS?.split(",") ?? [];
  try {
    const owner = json(resolve(directory, "owner.json"));
    if (inherited.includes(owner.token) && isDescendant(owner.owner))
      return action(env);
  } catch (error) {
    if (error.code !== "ENOENT") throw error;
  }
  const deadline = performance.now() + timeoutMs;
  const record = {
    schema: "midgard-resource/v1",
    resource,
    token: randomUUID(),
    owner: processIdentity(),
    tracksProcessGroups: true,
    root: realpathSync(process.cwd()),
    createdAt: new Date().toISOString(),
  };
  if (!record.owner)
    throw new Error(
      "resource ownership requires an attested Linux /proc process identity",
    );
  for (;;) {
    signal?.throwIfAborted();
    try {
      mkdirSync(directory, { mode: 0o700 });
      atomicJson(resolve(directory, "owner.json"), record);
      break;
    } catch (error) {
      if (error.code !== "EEXIST") throw error;
      // Never reap automatically: an owner might be between mkdir and its
      // record write. Explicit reclaim verifies a complete dead identity.
      if (performance.now() >= deadline)
        throw new Error(
          `resource busy: ${resource}; inspect with contrib resources list; reclaim only an abandoned lease`,
        );
      await delay(100, undefined, { signal });
    }
  }
  try {
    return await action({
      ...env,
      MIDGARD_CONTRIB_RESOURCE_TOKENS: [...inherited, record.token].join(","),
    });
  } finally {
    const current = json(resolve(directory, "owner.json"));
    if (current.token !== record.token || !sameProcess(current.owner))
      throw new Error(`resource owner changed: ${resource}; refusing cleanup`);
    if (groupsState(directory, record.token) !== "abandoned")
      throw new Error(
        `resource has unjoined child processes: ${resource}; refusing cleanup`,
      );
    rmSync(directory, { recursive: true });
  }
};

export const reclaimResource = (resource) => {
  const directory = resolve(resourceDirectory(), `${sha256(resource)}.lease`);
  const descriptor = openSync(directory, "r");
  const bound = `/proc/self/fd/${descriptor}`;
  try {
    const owner = json(resolve(bound, "owner.json"));
    const state = ownerState(owner, bound);
    if (owner.resource !== resource || state !== "abandoned")
      throw new Error(
        `refusing reclaim of live, unobservable or mismatched resource ${resource} (${state})`,
      );
    if (
      readdirSync(bound).some(
        (name) => !["owner.json", "processes"].includes(name),
      )
    )
      throw new Error("unknown lease contents; refusing reclaim");
    if (existsSync(resolve(bound, "processes"))) {
      for (const name of readdirSync(resolve(bound, "processes")))
        unlinkSync(resolve(bound, "processes", name));
      rmdirSync(resolve(bound, "processes"));
    }
    unlinkSync(resolve(bound, "owner.json"));
    rmdirSync(directory);
  } finally {
    closeSync(descriptor);
  }
};
