#!/usr/bin/env node
// Judges Aiken execution units against the committed script budgets
// (demo/midgard-core/src/script-budgets.json). Every budget is a ceiling: a
// fresh reading at or under it passes, so a cheaper script needs no edit.
//
//   node scripts/script-budgets.mjs check [--module <aiken-module>]...
//       [--plan <plan.json> --reports <directory>]
//   node scripts/script-budgets.mjs tighten [same options]
//   node scripts/script-budgets.mjs raise <budget name> --reason <text>
//   node scripts/script-budgets.mjs refit <budget name>
//
// Readings come from focused `aiken check` runs (run-focused-check.mjs), one
// per module and Aiken environment. With --plan and --reports, budgets in the
// census's environment are read from its complete, validated shard reports
// instead, and only the remaining environments are run.
//
// `tighten` lowers every passing ceiling that is more than 1% above its
// reading to the reading plus 1%. `raise` lifts one failing ceiling to its
// reading plus 1% and records why. `refit` settles one now-fits budget: it
// drops the aboveBasis record and lowers the ceiling to the reading plus 1%,
// capped at the basis. None of them touches a growth band.
//
// The plain form measures every budget with focused runs. CI's form reads
// the default-environment budgets from the shard census instead
// (.github/workflows/aiken-ci.yml, "Hold every script budget to its
// ceiling"), and runs the other environments the same way as the plain form.
// A census shard runs whole modules, a focused run only the budgeted tests,
// and a unit test reads the same units in either, so both forms judge the
// same budgets with the same verdicts. To judge a CI run's own readings,
// download its `aiken-shard-plan` artifact and every
// `aiken-shard-report-<N>` artifact (`gh run download <run id> -n <name>`),
// put the shard-<N>.json files in one directory, and pass the plan and that
// directory as --plan and --reports. That works only on the commit the run
// measured: the plan pins the source hash.

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { readShardUnion } from "./complete-test-shards.mjs";
import { focusedCheck } from "./run-focused-check.mjs";
import { SCRIPT_BUDGET_BASIS } from "../../../demo/midgard-core/src/script-budgets.ts";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const projectDirectory = resolve(scriptDirectory, "..");
export const TABLE_PATH = resolve(
  scriptDirectory,
  "../../../demo/midgard-core/src/script-budgets.json",
);

const DEFAULT_ENV = "default";
const AXES = ["mem", "cpu"];
const validTitle = /^[a-z0-9_]+$/u;
const validModule =
  /^[a-z0-9_]+(?:\.[a-z0-9_]+)*(?:\/[a-z0-9_]+(?:\.[a-z0-9_]+)*)*$/u;
const validEnv = /^[a-z0-9_]+$/u;

/** The reading plus 1%, rounded away from zero's side of the ceiling. */
export const withHeadroom = (units) => units + Math.ceil(Math.abs(units) / 100);

export const splitTest = (test) => {
  const cut = test.lastIndexOf("/");
  return { module: test.slice(0, cut), title: test.slice(cut + 1) };
};

/** The name a budget is filed under, derived from what it measures. */
export const budgetName = ({ test, minusControl, env }) =>
  `${env === undefined ? "" : `${env}:`}${test}${
    minusControl === undefined ? "" : ` - ${minusControl}`
  }`;

const isUnits = (value) =>
  value !== null &&
  typeof value === "object" &&
  AXES.every((axis) => Number.isSafeInteger(value[axis]));

const exceedsBasis = (units, basis) =>
  AXES.some((axis) => units[axis] > basis[axis]);

/** Every way the table is malformed, as messages; empty when it is sound. */
export const tableProblems = (table, basis) => {
  const problems = [];
  const budgets = table?.budgets;
  const growth = table?.growth ?? {};
  if (budgets === null || typeof budgets !== "object") {
    return ["the table has no budgets object"];
  }
  if (Object.keys(budgets).length === 0) problems.push("the table is empty");
  for (const [name, entry] of Object.entries(budgets)) {
    const at = `budget "${name}"`;
    if (typeof entry?.test !== "string" || !entry.test.includes("/")) {
      problems.push(`${at}: test must be "module/test"`);
      continue;
    }
    const { module, title } = splitTest(entry.test);
    if (!validModule.test(module) || !validTitle.test(title))
      problems.push(`${at}: "${entry.test}" is not an Aiken module/test`);
    if (
      entry.minusControl !== undefined &&
      (typeof entry.minusControl !== "string" ||
        !validTitle.test(entry.minusControl) ||
        entry.minusControl === title)
    )
      problems.push(`${at}: minusControl must be another test of the module`);
    if (
      entry.env !== undefined &&
      (typeof entry.env !== "string" ||
        !validEnv.test(entry.env) ||
        entry.env === DEFAULT_ENV)
    )
      problems.push(`${at}: env must name a non-default Aiken environment`);
    if (budgetName(entry) !== name)
      problems.push(`${at}: must be filed as "${budgetName(entry)}"`);
    if (!isUnits(entry.ceiling)) {
      problems.push(`${at}: ceiling must be integer mem and cpu`);
    } else if (entry.aboveBasis === undefined) {
      if (exceedsBasis(entry.ceiling, basis))
        problems.push(
          `${at}: ceiling is above the basis without an aboveBasis.why`,
        );
    } else if (
      typeof entry.aboveBasis?.why !== "string" ||
      entry.aboveBasis.why.length === 0
    ) {
      problems.push(`${at}: aboveBasis needs a why`);
    }
    if (
      entry.raisedBecause !== undefined &&
      (typeof entry.raisedBecause !== "string" ||
        entry.raisedBecause.length === 0)
    )
      problems.push(`${at}: raisedBecause must be a reason`);
    const known = new Set([
      "test",
      "minusControl",
      "env",
      "ceiling",
      "aboveBasis",
      "raisedBecause",
    ]);
    for (const key of Object.keys(entry))
      if (!known.has(key)) problems.push(`${at}: unknown field ${key}`);
  }
  for (const [name, claim] of Object.entries(growth)) {
    const at = `growth claim "${name}"`;
    for (const side of ["of", "from"])
      if (!Object.hasOwn(budgets, claim?.[side]))
        problems.push(`${at}: ${side} must name a budget`);
    for (const axis of AXES) {
      const band = claim?.within?.[axis];
      if (
        !Number.isSafeInteger(band?.min) ||
        !Number.isSafeInteger(band?.max) ||
        band.min > band.max
      )
        problems.push(`${at}: within.${axis} must be integer min <= max`);
    }
    if (typeof claim?.why !== "string" || claim.why.length === 0)
      problems.push(`${at}: needs a why`);
  }
  return problems;
};

const readingKey = (env, module, title) =>
  JSON.stringify([env ?? DEFAULT_ENV, module, title]);

/** The tests each budget needs, grouped by environment and module. */
export const requiredReadings = (budgets) => {
  const byEnv = new Map();
  for (const entry of budgets) {
    const env = entry.env ?? DEFAULT_ENV;
    const { module, title } = splitTest(entry.test);
    if (!byEnv.has(env)) byEnv.set(env, new Map());
    const modules = byEnv.get(env);
    if (!modules.has(module)) modules.set(module, new Set());
    modules.get(module).add(title);
    if (entry.minusControl !== undefined)
      modules.get(module).add(entry.minusControl);
  }
  return byEnv;
};

const measuredUnits = (entry, readings) => {
  const { module, title } = splitTest(entry.test);
  const env = entry.env ?? DEFAULT_ENV;
  const test = readings.get(readingKey(env, module, title));
  const missing = [];
  if (test === undefined) missing.push(title);
  let control;
  if (entry.minusControl !== undefined) {
    control = readings.get(readingKey(env, module, entry.minusControl));
    if (control === undefined) missing.push(entry.minusControl);
  }
  if (missing.length > 0) return { missing };
  if (control === undefined) return { units: { mem: test.mem, cpu: test.cpu } };
  return {
    units: { mem: test.mem - control.mem, cpu: test.cpu - control.cpu },
  };
};

/**
 * One verdict per budget and growth claim. `readings` maps
 * readingKey(env, module, title) to {mem, cpu}. Statuses: ok, over (above
 * the ceiling), missing (a test or control produced no reading), now-fits (an
 * aboveBasis reading inside the basis), out-of-band (growth outside its band).
 */
export const judgeBudgets = ({ table, basis, readings, names }) => {
  const verdicts = [];
  const selected =
    names === undefined ? Object.keys(table.budgets) : [...names];
  const measuredByName = new Map();
  for (const name of selected) {
    const entry = table.budgets[name];
    const { units, missing } = measuredUnits(entry, readings);
    if (missing !== undefined) {
      verdicts.push({
        name,
        status: "missing",
        detail: `no reading for ${missing.join(", ")}`,
      });
      continue;
    }
    measuredByName.set(name, units);
    const over = AXES.filter((axis) => units[axis] > entry.ceiling[axis]);
    if (over.length > 0) {
      verdicts.push({
        name,
        status: "over",
        measured: units,
        ceiling: entry.ceiling,
        detail: `above its ceiling on ${over.join(" and ")}`,
      });
    } else if (entry.aboveBasis !== undefined && !exceedsBasis(units, basis)) {
      verdicts.push({
        name,
        status: "now-fits",
        measured: units,
        ceiling: entry.ceiling,
        detail:
          "recorded as above the basis, but the reading now fits; run `refit` on it and revisit what it ruled out",
      });
    } else {
      verdicts.push({
        name,
        status: "ok",
        measured: units,
        ceiling: entry.ceiling,
      });
    }
  }
  for (const [name, claim] of Object.entries(table.growth ?? {})) {
    if (!selected.includes(claim.of) && !selected.includes(claim.from))
      continue;
    const of = measuredByName.get(claim.of);
    const from = measuredByName.get(claim.from);
    if (of === undefined || from === undefined) {
      verdicts.push({
        name,
        status: "missing",
        detail: `growth needs readings for "${claim.of}" and "${claim.from}"`,
      });
      continue;
    }
    const growth = { mem: of.mem - from.mem, cpu: of.cpu - from.cpu };
    const outside = AXES.filter(
      (axis) =>
        growth[axis] < claim.within[axis].min ||
        growth[axis] > claim.within[axis].max,
    );
    verdicts.push({
      name,
      status: outside.length === 0 ? "ok" : "out-of-band",
      measured: growth,
      ...(outside.length === 0
        ? {}
        : { detail: `growth outside its band on ${outside.join(" and ")}` }),
    });
  }
  return verdicts;
};

/** Lowers every passing ceiling more than 1% above its reading. */
export const tightenTable = (table, verdicts) => {
  const budgets = { ...table.budgets };
  const lowered = [];
  for (const verdict of verdicts) {
    if (verdict.status !== "ok" || !Object.hasOwn(budgets, verdict.name))
      continue;
    const entry = budgets[verdict.name];
    const ceiling = { ...entry.ceiling };
    for (const axis of AXES)
      ceiling[axis] = Math.min(
        ceiling[axis],
        withHeadroom(verdict.measured[axis]),
      );
    if (AXES.some((axis) => ceiling[axis] !== entry.ceiling[axis])) {
      budgets[verdict.name] = { ...entry, ceiling };
      lowered.push(verdict.name);
    }
  }
  return { table: { ...table, budgets }, lowered };
};

/** Lifts one over-ceiling budget to its reading plus 1%, with a reason. */
export const raiseBudget = ({ table, basis, name, verdict, reason }) => {
  const entry = table.budgets[name];
  if (entry === undefined)
    throw new Error(`no budget "${name}"; growth bands are edited by hand`);
  if (typeof reason !== "string" || reason.trim().length === 0)
    throw new Error("raise needs --reason");
  if (verdict?.status !== "over")
    throw new Error(
      `"${name}" is ${verdict?.status ?? "unmeasured"}, not over its ceiling; only an over-ceiling budget is raised`,
    );
  if (entry.aboveBasis === undefined && exceedsBasis(verdict.measured, basis))
    throw new Error(
      `"${name}" now reads above the basis; shrink the script, or record why it may exceed the basis (aboveBasis.why) by hand`,
    );
  const ceiling = {};
  for (const axis of AXES) {
    let raised = Math.max(
      entry.ceiling[axis],
      withHeadroom(verdict.measured[axis]),
    );
    if (entry.aboveBasis === undefined) raised = Math.min(raised, basis[axis]);
    ceiling[axis] = raised;
  }
  return {
    ...table,
    budgets: {
      ...table.budgets,
      [name]: { ...entry, ceiling, raisedBecause: reason.trim() },
    },
  };
};

/**
 * Settles one now-fits budget: drops its aboveBasis record and lowers its
 * ceiling to the reading plus 1%, capped at the basis.
 */
export const refitBudget = ({ table, basis, name, verdict }) => {
  const entry = table.budgets[name];
  if (entry === undefined) throw new Error(`no budget "${name}"`);
  if (verdict?.status !== "now-fits")
    throw new Error(
      `"${name}" is ${verdict?.status ?? "unmeasured"}, not now-fits; only a budget recorded above the basis whose reading fits is refit`,
    );
  const { aboveBasis: _settled, ...rest } = entry;
  const ceiling = {};
  for (const axis of AXES)
    ceiling[axis] = Math.min(
      entry.ceiling[axis],
      withHeadroom(verdict.measured[axis]),
      basis[axis],
    );
  return {
    ...table,
    budgets: { ...table.budgets, [name]: { ...rest, ceiling } },
  };
};

// ---------------------------------------------------------------- readings

const readFocused = (env, module, titles, readings, errors) => {
  const result = focusedCheck({
    projectDirectory,
    moduleName: module,
    testNames: [...titles].sort(),
    environment: env === DEFAULT_ENV ? undefined : env,
  });
  if (result.status !== 0) {
    errors.push(`${env} ${module}: ${result.stderr.trim()}`);
    return;
  }
  const report = JSON.parse(result.stdout);
  for (const test of report.modules[0].tests) {
    if (isUnits(test.execution_units))
      readings.set(readingKey(env, module, test.title), test.execution_units);
  }
};

const readCensus = (plan, reportsDirectory, readings) => {
  const { ids } = readShardUnion({ plan, projectDirectory, reportsDirectory });
  const env = plan.environment ?? DEFAULT_ENV;
  for (const [id, test] of ids) {
    const [module, title] = JSON.parse(id);
    if (isUnits(test.execution_units))
      readings.set(readingKey(env, module, title), test.execution_units);
  }
  return env;
};

const collectReadings = (budgets, census) => {
  const readings = new Map();
  const errors = [];
  const censusEnv =
    census === undefined
      ? undefined
      : readCensus(census.plan, census.reportsDirectory, readings);
  for (const [env, modules] of requiredReadings(budgets)) {
    if (env === censusEnv) continue;
    for (const [module, titles] of modules) {
      process.stderr.write(`measuring ${env} ${module} (${titles.size})\n`);
      readFocused(env, module, titles, readings, errors);
    }
  }
  return { readings, errors };
};

// ---------------------------------------------------------------- CLI

const usage = `usage:
  node scripts/script-budgets.mjs check [--module <aiken-module>]... [--plan <plan.json> --reports <dir>]
  node scripts/script-budgets.mjs tighten [--module <aiken-module>]... [--plan <plan.json> --reports <dir>]
  node scripts/script-budgets.mjs raise <budget name> --reason <text>
  node scripts/script-budgets.mjs refit <budget name>`;

const parseOptions = (args) => {
  const options = { modules: [], plan: undefined, reports: undefined };
  for (let index = 0; index < args.length; index += 2) {
    const [flag, value] = [args[index], args[index + 1]];
    if (value === undefined) throw new Error(usage);
    if (flag === "--module") options.modules.push(value);
    else if (flag === "--plan" && options.plan === undefined)
      options.plan = value;
    else if (flag === "--reports" && options.reports === undefined)
      options.reports = value;
    else throw new Error(usage);
  }
  if ((options.plan === undefined) !== (options.reports === undefined))
    throw new Error("--plan and --reports go together");
  return options;
};

const formatUnits = (units) =>
  units === undefined ? "" : `${units.mem} mem / ${units.cpu} cpu`;

const printVerdicts = (verdicts) => {
  for (const verdict of verdicts) {
    const line = [
      verdict.status.padEnd(11),
      verdict.name,
      verdict.measured === undefined
        ? ""
        : `read ${formatUnits(verdict.measured)}`,
      verdict.ceiling === undefined
        ? ""
        : `ceiling ${formatUnits(verdict.ceiling)}`,
      verdict.detail ?? "",
    ]
      .filter((part) => part.length > 0)
      .join("  ");
    (verdict.status === "ok" ? process.stdout : process.stderr).write(
      `${line}\n`,
    );
  }
};

const loadTable = () => JSON.parse(readFileSync(TABLE_PATH, "utf8"));

const writeTable = (table) =>
  writeFileSync(TABLE_PATH, `${JSON.stringify(table, null, 2)}\n`);

const run = (args) => {
  const [command, ...rest] = args;
  const table = loadTable();
  const problems = tableProblems(table, SCRIPT_BUDGET_BASIS);
  if (problems.length > 0) {
    for (const problem of problems) process.stderr.write(`${problem}\n`);
    return 2;
  }
  let names;
  let options;
  let raiseName;
  let reason;
  let refitName;
  if (command === "check" || command === "tighten") {
    options = parseOptions(rest);
    names = Object.keys(table.budgets).filter(
      (name) =>
        options.modules.length === 0 ||
        options.modules.includes(splitTest(table.budgets[name].test).module),
    );
    for (const module of options.modules)
      if (
        !names.some(
          (name) => splitTest(table.budgets[name].test).module === module,
        )
      ) {
        process.stderr.write(`no budget measures module ${module}\n`);
        return 1;
      }
  } else if (command === "raise") {
    [raiseName] = rest;
    if (rest.length !== 3 || rest[1] !== "--reason") throw new Error(usage);
    reason = rest[2];
    if (!Object.hasOwn(table.budgets, raiseName)) {
      process.stderr.write(`no budget "${raiseName}"\n`);
      return 2;
    }
    names = [raiseName];
    options = { modules: [] };
  } else if (command === "refit") {
    [refitName] = rest;
    if (rest.length !== 1) throw new Error(usage);
    if (!Object.hasOwn(table.budgets, refitName)) {
      process.stderr.write(`no budget "${refitName}"\n`);
      return 2;
    }
    names = [refitName];
    options = { modules: [] };
  } else {
    throw new Error(usage);
  }
  if (names.length === 0) {
    process.stderr.write("no budgets selected\n");
    return 1;
  }
  const selected = names.map((name) => table.budgets[name]);
  const census =
    options.plan === undefined
      ? undefined
      : {
          plan: JSON.parse(readFileSync(options.plan, "utf8")),
          reportsDirectory: options.reports,
        };
  const { readings, errors } = collectReadings(selected, census);
  for (const error of errors) process.stderr.write(`${error}\n`);
  const verdicts = judgeBudgets({
    table,
    basis: SCRIPT_BUDGET_BASIS,
    readings,
    names,
  });
  printVerdicts(verdicts);
  const failed = verdicts.filter((verdict) => verdict.status !== "ok");

  if (command === "raise") {
    writeTable(
      raiseBudget({
        table,
        basis: SCRIPT_BUDGET_BASIS,
        name: raiseName,
        verdict: verdicts.find((verdict) => verdict.name === raiseName),
        reason,
      }),
    );
    process.stdout.write(`raised ${raiseName}\n`);
    return 0;
  }
  if (command === "refit") {
    writeTable(
      refitBudget({
        table,
        basis: SCRIPT_BUDGET_BASIS,
        name: refitName,
        verdict: verdicts.find((verdict) => verdict.name === refitName),
      }),
    );
    process.stdout.write(`refit ${refitName}\n`);
    return 0;
  }
  if (command === "tighten") {
    const { table: tightened, lowered } = tightenTable(table, verdicts);
    if (lowered.length > 0) writeTable(tightened);
    process.stdout.write(`lowered ${lowered.length} ceiling(s)\n`);
  }
  process.stdout.write(
    `${verdicts.length - failed.length}/${verdicts.length} script budgets hold\n`,
  );
  if (failed.length > 0)
    process.stderr.write(
      "A script budget failed. Shrink the script, or if the cost is accepted run `node onchain/aiken/scripts/script-budgets.mjs raise <name> --reason <why>`.\n",
    );
  return errors.length === 0 && failed.length === 0 ? 0 : 1;
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  try {
    process.exitCode = run(process.argv.slice(2));
  } catch (error) {
    process.stderr.write(
      `${error instanceof Error ? error.message : String(error)}\n`,
    );
    process.exitCode = 2;
  }
}
