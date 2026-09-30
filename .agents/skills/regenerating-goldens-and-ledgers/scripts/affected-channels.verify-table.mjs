import {
  claims,
  outsideDiscovery,
  packageScriptPattern,
  unclaimed,
  writerEnvPattern,
} from "./affected-channels.channel-triggers.mjs";
import {
  expandSets,
  isGlob,
  ledgerModules,
  matchesGlob,
  workspace,
} from "./affected-channels.parse-arguments.mjs";
import {
  commandTokens,
  packageScripts,
  parsePackageScript,
  verifyCommand,
  workflowStep,
} from "./affected-channels.verify-command.mjs";

export const verifyTable = (tree, table) => {
  const problems = [];
  const tracked = tree.trackedFiles();
  const trackedSet = new Set(tracked);
  const ids = new Set();
  const problem = (channel, message) =>
    problems.push(`${channel ? `[${channel.id}] ` : ""}${message}`);
  const matchesSomething = (glob) =>
    isGlob(glob)
      ? tracked.some((file) => matchesGlob(file, glob))
      : trackedSet.has(glob) || tree.exists(glob);

  for (const [name, patterns] of Object.entries(table.inputSets)) {
    for (const pattern of patterns) {
      if (!pattern.startsWith("@") && !matchesSomething(pattern))
        problem(undefined, `input set ${name}: ${pattern} matches nothing`);
    }
  }

  for (const channel of table.channels) {
    if (!channel.id) {
      problem(undefined, "a channel has no id");
      continue;
    }
    if (ids.has(channel.id)) problem(channel, "duplicate channel id");
    ids.add(channel.id);
    if (!channel.summary) problem(channel, "no summary");
    for (const generator of channel.generators ?? []) {
      if (!trackedSet.has(generator))
        problem(channel, `generator ${generator} is not a tracked file`);
    }
    for (const pattern of channel.outputs ?? []) {
      if (!matchesSomething(pattern))
        problem(channel, `output ${pattern} matches no tracked file`);
    }
    if (!channel.outputs?.length) problem(channel, "no outputs");
    let inputs = [];
    try {
      inputs = [
        ...expandSets(channel.inputs, table),
        ...expandSets(channel.exclude, table),
      ];
    } catch (error) {
      problem(channel, error.message);
    }
    for (const pattern of inputs) {
      if (!matchesSomething(pattern))
        problem(channel, `input ${pattern} matches nothing`);
    }
    for (const directory of channel.producers ?? []) {
      if (!workspace(tree).manifests.has(directory))
        problem(channel, `producer ${directory} is not a workspace package`);
    }
    for (const root of channel.aikenClosure ?? []) {
      if (!trackedSet.has(root))
        problem(channel, `Aiken closure root ${root} is not tracked`);
    }
    if (channel.ledger) {
      if (!trackedSet.has(channel.ledger))
        problem(channel, `ledger ${channel.ledger} is not tracked`);
      const modules = ledgerModules(tree, channel.ledger);
      if (!modules.length)
        problem(channel, `ledger ${channel.ledger} lists no measured modules`);
      for (const { name, file } of modules) {
        if (!file)
          problem(channel, `measured module ${name} resolves to no .ak file`);
      }
    }
    for (const reference of channel.packageScripts ?? []) {
      const { directory, script } = parsePackageScript(reference);
      const scripts = packageScripts(tree, directory);
      if (!scripts || !(script in scripts))
        problem(channel, `package script ${reference} does not exist`);
    }
    for (const name of channel.writerEnv ?? []) {
      const sources = [
        ...(channel.generators ?? []),
        ...inputs.filter((p) => !isGlob(p)),
      ];
      const named = sources.some((file) => tree.read(file)?.includes(name));
      if (!named)
        problem(
          channel,
          `writer env ${name} appears in none of the channel's generators or inputs`,
        );
    }
    for (const mode of ["sync", "check"]) {
      const entry = channel[mode];
      if (!entry || (!entry.run && !entry.manual && !entry.none)) {
        problem(channel, `${mode} needs one of run, manual or none`);
        continue;
      }
      if (entry.run)
        for (const message of verifyCommand(tree, entry.run))
          problem(channel, `${mode}: ${message}`);
    }
    if (
      !channel.ci ||
      (!channel.ci.none && !(channel.ci.workflow && channel.ci.step))
    ) {
      problem(channel, "ci needs workflow and step, or none");
    } else if (channel.ci.workflow) {
      const source = tree.read(channel.ci.workflow);
      const step =
        source === undefined
          ? undefined
          : workflowStep(source, channel.ci.step);
      if (source === undefined)
        problem(channel, `workflow ${channel.ci.workflow} does not exist`);
      else if (step === undefined)
        problem(
          channel,
          `no step "${channel.ci.step}" in ${channel.ci.workflow}`,
        );
      else if (channel.ci.via) {
        const packageDirectory = /^(demo\/[^/]+)\//u.exec(channel.ci.via)?.[1];
        if (!trackedSet.has(channel.ci.via))
          problem(channel, `ci.via ${channel.ci.via} is not tracked`);
        if (!packageDirectory || !step.includes(`--dir ${packageDirectory}`)) {
          problem(
            channel,
            `step "${channel.ci.step}" does not run the package that holds ${channel.ci.via}`,
          );
        }
      } else if (
        !commandTokens(channel).some((token) => step.includes(token))
      ) {
        problem(
          channel,
          `step "${channel.ci.step}" mentions none of the channel's check command, generators or outputs`,
        );
      }
    }
    for (const id of channel.then ?? []) {
      if (!table.channels.some((c) => c.id === id))
        problem(channel, `then names unknown channel ${id}`);
    }
    // Every Aiken path a generator names must be one of its outputs, so a
    // generator that starts writing a new module cannot drift out of the table.
    for (const generator of channel.generators ?? []) {
      const source = tree.read(generator);
      if (source === undefined) continue;
      const code = source
        .replace(/\/\*[\s\S]*?\*\//gu, "")
        .replace(/^\s*\/\/.*$/gmu, "");
      for (const match of code.matchAll(
        /["'`](onchain\/aiken\/[^"'`]+\.ak)["'`]/gu,
      )) {
        if (
          !(channel.outputs ?? []).some((glob) => matchesGlob(match[1], glob))
        ) {
          problem(
            channel,
            `generator ${generator} names ${match[1]}, which is not among the channel's outputs`,
          );
        }
      }
    }
  }

  for (const entry of table.ignored) {
    if (!entry.why)
      problem(
        undefined,
        `ignored entry ${entry.path ?? entry.packageScript} has no reason`,
      );
    if (entry.path && !trackedSet.has(entry.path))
      problem(undefined, `ignored path ${entry.path} is gone; drop the entry`);
    if (entry.packageScript) {
      const { directory, script } = parsePackageScript(entry.packageScript);
      const scripts = packageScripts(tree, directory);
      if (!scripts || !(script in scripts))
        problem(
          undefined,
          `ignored package script ${entry.packageScript} is gone; drop the entry`,
        );
    }
  }

  // Coverage: everything in the tree that looks generated or generating.
  const claimed = claims(table);
  problems.push(...unclaimed(tree, table, tracked));
  for (const directory of ["demo", ...workspace(tree).manifests.keys()]) {
    const scripts = packageScripts(tree, directory);
    if (!scripts) continue;
    for (const script of Object.keys(scripts)) {
      if (!packageScriptPattern.test(script)) continue;
      if (!claimed.packageScripts.has(`${directory}:${script}`)) {
        problems.push(
          `package script ${directory}:${script} looks like a generator or check but no channel claims it`,
        );
      }
    }
  }
  const discoveredEnv = new Set();
  for (const file of tracked) {
    if (
      !file.startsWith("demo/") ||
      outsideDiscovery(file) ||
      !/\.(ts|mts|mjs|js)$/u.test(file)
    )
      continue;
    for (const match of (tree.read(file) ?? "").matchAll(writerEnvPattern))
      discoveredEnv.add(match[1]);
  }
  for (const name of [...discoveredEnv].sort()) {
    if (!claimed.writerEnv.has(name))
      problems.push(
        `writer env ${name} is used in demo/ but no channel claims it`,
      );
  }
  return problems;
};

// ------------------------------------------------------------------ output

const describeCi = (ci) =>
  ci.none
    ? `none: ${ci.none}`
    : `${ci.workflow} / ${ci.step}${ci.via ? ` (via ${ci.via})` : ""}`;

const describeMode = (entry) =>
  entry.run ??
  (entry.manual ? `manual: ${entry.manual}` : `none: ${entry.none}`);

export const renderMapping = ({ source, files, affected, problems }) => {
  const lines = [
    `affected-channels: ${files.length} changed file(s) ${source}`,
  ];
  if (!affected.length)
    lines.push("looked: no generated-artifact channel is affected.");
  else {
    lines.push(`${affected.length} channel(s) affected, in run order:`);
    for (const { channel, reasons } of affected) {
      lines.push("", `${channel.id}  [${channel.kind ?? "channel"}]`);
      const shown = reasons.slice(0, 3);
      for (const reason of shown) lines.push(`  because  ${reason}`);
      if (reasons.length > shown.length)
        lines.push(`  because  ... and ${reasons.length - shown.length} more`);
      lines.push(`  check    ${describeMode(channel.check)}`);
      lines.push(`  sync     ${describeMode(channel.sync)}`);
      lines.push(`  ci       ${describeCi(channel.ci)}`);
    }
  }
  for (const message of problems) lines.push(`PROBLEM: ${message}`);
  return lines.join("\n");
};
