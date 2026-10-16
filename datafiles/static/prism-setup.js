window.Prism = { manual: true };
document.addEventListener('DOMContentLoaded', function () {
  if (!Prism.plugins || !Prism.plugins.autoloader) return;
  Prism.plugins.autoloader.languages_path = 'https://cdn.jsdelivr.net/npm/prismjs@1.30.0/components/';
  Prism.highlightAll();
});
