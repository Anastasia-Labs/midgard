#!/usr/bin/env node
// Checks the agent skills under .agents/skills, and the .claude/skills link
// that points the .claude/ skill lookup at the same tree.
//
// It checks what can rot without anyone noticing:
//   - .claude/skills is a symlink to ../.agents/skills;
//   - every skill directory has a SKILL.md whose frontmatter `name` equals the
//     directory name and whose `description` is present and at most 1,024
//     characters, the limit past which the harness truncates it;
//   - SKILL.md stays within 500 lines; longer material belongs in references/;
//   - every relative Markdown link in a skill's .md files resolves.
//
// It does not see backticked paths (`docs/agents/x.md`) or whether a skill's
// instructions are still true. A skill can pass this check and be wrong.
//
// Usage: node scripts/ci/check-agent-skills.mjs [repository-root]

import {
  existsSync,
  lstatSync,
  readdirSync,
  readFileSync,
  readlinkSync,
} from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const SKILLS_DIR = ".agents/skills";
const CLAUDE_LINK = ".claude/skills";
const CLAUDE_LINK_TARGET = "../.agents/skills";
const MAX_DESCRIPTION = 1024;
const MAX_SKILL_LINES = 500;
const NAME_SHAPE = /^[a-z0-9]+(?:-[a-z0-9]+)*$/u;

const parseFrontmatter = (text) => {
  const match = /^---\r?\n([\s\S]*?)\r?\n---\r?\n/u.exec(text);
  if (match === null) return null;
  const fields = new Map();
  for (const line of match[1].split(/\r?\n/u)) {
    const field = /^([A-Za-z_][\w-]*):\s*(.*)$/u.exec(line);
    if (field === null) continue;
    fields.set(field[1], field[2].replace(/^(["'])(.*)\1$/u, "$2").trim());
  }
  return fields;
};

const markdownFiles = (directory) =>
  readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) return markdownFiles(path);
    return entry.name.endsWith(".md") ? [path] : [];
  });

const brokenLinks = (file, root) => {
  // Fenced code is example text, not navigation.
  const text = readFileSync(file, "utf8").replace(/^```[\s\S]*?^```/gmu, "");
  const problems = [];
  for (const [, target] of text.matchAll(/\]\(([^)\s]+)(?:\s+"[^"]*")?\)/gu)) {
    if (/^(?:[a-z][a-z0-9+.-]*:|#)/iu.test(target)) continue;
    const path = decodeURIComponent(target.split("#")[0]);
    if (!existsSync(resolve(dirname(file), path))) {
      problems.push(`${relative(root, file)}: broken link ${target}`);
    }
  }
  return problems;
};

export const checkAgentSkills = (root) => {
  const problems = [];

  const link = join(root, CLAUDE_LINK);
  const linkStat = lstatSync(link, { throwIfNoEntry: false });
  if (linkStat === undefined) {
    problems.push(
      `${CLAUDE_LINK} is missing; the skills are not discoverable under .claude/`,
    );
  } else if (!linkStat.isSymbolicLink()) {
    problems.push(
      `${CLAUDE_LINK} must be a symlink to ${CLAUDE_LINK_TARGET}, not a copy`,
    );
  } else if (readlinkSync(link) !== CLAUDE_LINK_TARGET) {
    problems.push(
      `${CLAUDE_LINK} points at ${readlinkSync(link)}, expected ${CLAUDE_LINK_TARGET}`,
    );
  }

  const skillsRoot = join(root, SKILLS_DIR);
  if (!existsSync(skillsRoot)) return [...problems, `${SKILLS_DIR} is missing`];

  for (const entry of readdirSync(skillsRoot, { withFileTypes: true })) {
    if (!entry.isDirectory()) {
      problems.push(
        `${SKILLS_DIR}/${entry.name}: only skill directories belong here`,
      );
      continue;
    }
    const where = `${SKILLS_DIR}/${entry.name}`;
    const skillFile = join(skillsRoot, entry.name, "SKILL.md");
    if (!existsSync(skillFile)) {
      problems.push(`${where}: has no SKILL.md`);
      continue;
    }
    const text = readFileSync(skillFile, "utf8");
    const fields = parseFrontmatter(text);
    if (fields === null) {
      problems.push(`${where}/SKILL.md: must open with --- frontmatter ---`);
    } else {
      const name = fields.get("name") ?? "";
      const description = fields.get("description") ?? "";
      if (name !== entry.name) {
        problems.push(
          `${where}/SKILL.md: name "${name}" must equal the directory name`,
        );
      }
      if (!NAME_SHAPE.test(entry.name)) {
        problems.push(`${where}: directory name must be lowercase kebab-case`);
      }
      if (description === "") {
        problems.push(`${where}/SKILL.md: description is missing`);
      } else if (description.length > MAX_DESCRIPTION) {
        problems.push(
          `${where}/SKILL.md: description is ${description.length} characters, limit ${MAX_DESCRIPTION}`,
        );
      }
    }
    const lines = text.split("\n").length;
    if (lines > MAX_SKILL_LINES) {
      problems.push(
        `${where}/SKILL.md: ${lines} lines, limit ${MAX_SKILL_LINES}; move detail into references/`,
      );
    }
    for (const file of markdownFiles(join(skillsRoot, entry.name))) {
      problems.push(...brokenLinks(file, root));
    }
  }
  return problems;
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const root = resolve(
    process.argv[2] ?? join(dirname(fileURLToPath(import.meta.url)), "../.."),
  );
  const problems = checkAgentSkills(root);
  if (problems.length > 0) {
    console.error(`agent skills: ${problems.length} problem(s)`);
    for (const problem of problems) console.error(`  ${problem}`);
    process.exit(1);
  }
  const count = readdirSync(join(root, SKILLS_DIR)).length;
  console.log(
    `agent skills: ${count} skills checked, ${CLAUDE_LINK} link intact`,
  );
}
