// paste this snippet into the developer console
// to get the German House recipes as scheme code
//
;(function extractAndLogRecipesOnly(){
  const recipes = [];
  const slugify = s => s.trim().toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/(^-|-$)/g, '');

  // 1. Gather each recipe’s items
  document.querySelectorAll('.recipe-container').forEach(card => {
    const title = card.querySelector('.card-header h2')?.textContent || '';
    const recipeSlug = slugify(title);

    const items = Array.from(card.querySelectorAll('tr.quantity')).map(tr => {
      const mag  = tr.querySelector('.magnitude').textContent.trim();
      const unit = tr.querySelector('.unit').textContent.trim();
      const orig = tr.children[2].textContent.trim();
      return { orig, mag, unit };
    });

    if (items.length) {
      recipes.push({ recipeSlug, items });
    }
  });

  // 2. Build output lines for recipes only
  const lines = [];
  recipes.forEach(({ recipeSlug, items }) => {
    lines.push(`(define ${recipeSlug}-recipe`);
    lines.push(`  (list`);
    items.forEach(({ orig, mag, unit }) => {
      lines.push(`    (make-recipe-item "${orig}" ${mag} '${unit})`);
    });
    lines.push(`  ))`);
    lines.push('');  // blank line between recipes
  });

  // 3. Single console.log for easy copy
  console.log(lines.join('\n'));
})();
