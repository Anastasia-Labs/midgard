import assert from "node:assert/strict";
import {
  mkdirSync,
  mkdtempSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

import { checkAgentSkills } from "./check-agent-skills.mjs";

const skill = (name, description, body = "Body.\n") =>
  `---\nname: ${name}\ndescription: ${description}\n---\n\n${body}`;

// Builds a repository root holding the given skills ({dir: SKILL.md text}) and,
// unless told otherwise, a correct .claude/skills link.
const withRepo = (skills, callback, { link = "../.agents/skills" } = {}) => {
  const root = mkdtempSync(join(tmpdir(), "midgard-agent-skills-"));
  try {
    mkdirSync(join(root, ".agents/skills"), { recursive: true });
    mkdirSync(join(root, ".claude"), { recursive: true });
    if (link !== null) symlinkSync(link, join(root, ".claude/skills"));
    for (const [dir, text] of Object.entries(skills)) {
      mkdirSync(join(root, ".agents/skills", dir, "references"), {
        recursive: true,
      });
      writeFileSync(join(root, ".agents/skills", dir, "SKILL.md"), text);
    }
    return callback(root);
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
};

const expectOnly = (problems, pattern) => {
  assert.equal(problems.length, 1, problems.join("\n"));
  assert.match(problems[0], pattern);
};

test("a well-formed skill tree passes", () => {
  withRepo(
    { "doing-things": skill("doing-things", "Use when doing things.") },
    (root) => {
      assert.deepEqual(checkAgentSkills(root), []);
    },
  );
});

test("a missing .claude/skills link is refused", () => {
  withRepo(
    { "doing-things": skill("doing-things", "Use when.") },
    (root) =>
      expectOnly(checkAgentSkills(root), /\.claude\/skills is missing/u),
    { link: null },
  );
});

test("a link to the wrong place is refused, even when the target exists", () => {
  withRepo(
    { "doing-things": skill("doing-things", "Use when.") },
    (root) =>
      expectOnly(checkAgentSkills(root), /points at \.\.\/\.agents, expected/u),
    { link: "../.agents" },
  );
});

test("a name that differs from its directory is refused", () => {
  withRepo({ "doing-things": skill("do-things", "Use when.") }, (root) => {
    expectOnly(
      checkAgentSkills(root),
      /name "do-things" must equal the directory name/u,
    );
  });
});

test("missing frontmatter, description and SKILL.md are each refused", () => {
  withRepo(
    {
      "no-front": "# No frontmatter\n",
      "no-description": "---\nname: no-description\n---\n",
    },
    (root) => {
      mkdirSync(join(root, ".agents/skills/empty-dir"));
      const problems = checkAgentSkills(root).join("\n");
      assert.match(
        problems,
        /no-front\/SKILL\.md: must open with --- frontmatter/u,
      );
      assert.match(
        problems,
        /no-description\/SKILL\.md: description is missing/u,
      );
      assert.match(problems, /empty-dir: has no SKILL\.md/u);
    },
  );
});

test("an over-long description and an over-long SKILL.md are refused", () => {
  withRepo(
    {
      "long-description": skill("long-description", "x".repeat(1025)),
      "long-body": skill("long-body", "Use when.", "line\n".repeat(500)),
    },
    (root) => {
      const problems = checkAgentSkills(root).join("\n");
      assert.match(problems, /description is 1025 characters, limit 1024/u);
      assert.match(problems, /long-body\/SKILL\.md: \d+ lines, limit 500/u);
    },
  );
});

test("broken relative links are refused; external, anchor and fenced links are not", () => {
  const body = [
    "[ok](references/notes.md#section) [web](https://example.com) [here](#top)",
    "[gone](references/missing.md)",
    "```",
    "[example](not/a/real/file.md)",
    "```",
  ].join("\n");
  withRepo(
    { "linking-things": skill("linking-things", "Use when.", body) },
    (root) => {
      writeFileSync(
        join(root, ".agents/skills/linking-things/references/notes.md"),
        "notes\n",
      );
      expectOnly(
        checkAgentSkills(root),
        /SKILL\.md: broken link references\/missing\.md/u,
      );
    },
  );
});

test("a raw Vitest command in a fenced block is refused; prose, contrib and a justified line are not", () => {
  const body = [
    "Prose may name `vitest run` and pnpm exec vitest.",
    "```sh",
    "node scripts/contrib.mjs test --package midgard-node --file tests/a.test.ts",
    "pnpm --dir demo/midgard-node exec vitest run tests/a.test.ts",
    "npx vitest tests/b.test.ts",
    "pnpm vitest",
    "pnpm exec vitest run x # raw-vitest-ok: contrib cannot load this config",
    "pnpm exec vitest run x # raw-vitest-ok:",
    "```",
    "1. A step.",
    "",
    "   ```bash",
    '   MIDGARD_X=1 pnpm --dir "$ROOT/demo/x" \\',
    "     exec vitest run tests/c.test.ts",
    "   ```",
  ].join("\n");
  withRepo(
    { "testing-things": skill("testing-things", "Use when.", body) },
    (root) => {
      writeFileSync(
        join(root, ".agents/skills/testing-things/references/notes.md"),
        "```\nvitest run tests/d.test.ts\n```\n",
      );
      const problems = checkAgentSkills(root);
      assert.deepEqual(
        problems.map((problem) => problem.split(": raw Vitest")[0]),
        [
          ".agents/skills/testing-things/SKILL.md:9",
          ".agents/skills/testing-things/SKILL.md:10",
          ".agents/skills/testing-things/SKILL.md:11",
          ".agents/skills/testing-things/SKILL.md:13",
          ".agents/skills/testing-things/SKILL.md:19",
          ".agents/skills/testing-things/references/notes.md:2",
        ],
        problems.join("\n"),
      );
      assert.match(
        problems[0],
        /node scripts\/contrib\.mjs test --package NAME/u,
      );
    },
  );
});
