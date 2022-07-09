{
  description = "jedi dotfiles";

  outputs = { self, ... }: {
    templates = {

      simple = {
        path = ./templates/simple;
        description = "Simple environment.";
      };
    };

    defaultTemplate = self.templates.simple;
  };
}
