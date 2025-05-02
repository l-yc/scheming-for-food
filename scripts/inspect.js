// paste this snippet into the developer console
// to get the German House recipes as scheme code
;(function extractAndLogRecipesWithProvenance(){
  const recipes = [];
  const slugify = s => s.trim().toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/(^-|-$)/g, '');

  // 1. Gather each recipe’s title, id, items, and provenance URL
  document.querySelectorAll('.recipe-container').forEach(card => {
    const titleEl = card.querySelector('.card-header h2');
    if (!titleEl || !card.id) return;

    const titleText  = titleEl.textContent.trim();
    const recipeSlug = slugify(titleText);
    const idNum      = card.id.replace(/^recipe_/, '');
    const provenance = `${window.location.origin}/recipes/${idNum}/`;

    const items = Array.from(card.querySelectorAll('tr.quantity')).map(tr => {
      const mag  = tr.querySelector('.magnitude').textContent.trim();
      const unit = tr.querySelector('.unit').textContent.trim();
      const orig = tr.children[2].textContent.trim();
      return { orig, mag, unit };
    });

    if (items.length) {
      recipes.push({ recipeSlug, titleText, items, provenance });
    }
  });

  // 2. Build output lines using the new %make-recipe format
  const lines = [];
  recipes.forEach(({ recipeSlug, titleText, items, provenance }) => {
    lines.push(`(define ${recipeSlug}-recipe`);
    lines.push(`  (%make-recipe`);
    lines.push(`   "${titleText}"`);
    lines.push(`   (list`);
    items.forEach(({ orig, mag, unit }) => {
      lines.push(`    (make-recipe-item "${orig}" ${mag} '${unit})`);
    });
    lines.push(`   )`);
    lines.push(`   "${provenance}"`);
    lines.push(`  )`);
    lines.push(`)`);
    lines.push('');  // blank line between recipes
  });

  // 3. Define a master list of all recipes
  if (recipes.length) {
    const names = recipes.map(r => `${r.recipeSlug}-recipe`);
    lines.push(`(define recipes`);
    lines.push(`  (list ${names.join('\n\t')})`);
  }

  // 4. Single console.log for easy copy
  console.log(lines.join('\n'));
})();

