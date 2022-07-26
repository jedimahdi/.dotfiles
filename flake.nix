{
  description = "jedi dotfiles";

  outputs = { self, ... }: {
    templates = {

      simple = {
        path = ./templates/simple;
        description = "Simple environment.";
      };

      rust = {
        path = ./templates/rust;
        description = "Rust environment.";
      };

      node = {
        path = ./templates/node;
        description = "Node environment.";
      };
    };

    defaultTemplate = self.templates.simple;
  };
}
