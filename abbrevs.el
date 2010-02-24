(require 'snippet)

(defun merge-abbrev-tables (old new)
  (if old
      (mapatoms 
       (function 
        (lambda (symbol)
          (or (intern-soft (symbol-name symbol) new)
              (define-abbrev new (symbol-name symbol)
                (symbol-value symbol) (symbol-function symbol)))))
       old)))

(define-abbrev-table 'html-mode-abbrev-table ())

(snippet-with-abbrev-table 
 'html-mode-abbrev-table 
 ("div"             . "<div>$.</div>")
 ("span"            . "<span>$.</span>")
 ("form"            . "<form action=\"$${action}\" method=\"$${post}\">$.</form>")
 ("input"           . "<input type=\"$${text}\" name=\"$${name}\" value=\"$${value}\"/>")
 ("a"               . "<a href=\"$${href}\">$.</a>")
 ("br"              . "<br/>$.")
 ("ul"              . "<ul>$.</ul>")
 ("ol"              . "<ul>$.</ul>")
 ("li"              . "<li>$.</li>")
 ("tab"             . "<table>$.</table>")
 ("tr"              . "<tr>$.</tr>")
 ("td"              . "<td>$.</td>")
 ("th"              . "<th>$.</th>")
 ("str"             . "<strong>$.</strong>")
 ("em"              . "<em>$.</em>")
 ("meta"            . "<meta name=\"$${name}\" content=\"$${content}\"/>")
 ("style"           . "<style type=\"text/css\">$.</style>")
 ("script"          . "<script type=\"text/javascript\">$.</script>")
 ("img"             . "<img src=\"$.\"/>")
 ("link"            . "<link href=\"$${href}\" media=\"screen\" rel=\"stylesheet\" type=\"text/css\"/>"))

(define-abbrev-table 'rhtml-mode-abbrev-table ())
(merge-abbrev-tables html-mode-abbrev-table rhtml-mode-abbrev-table)

(snippet-with-abbrev-table 
 'rhtml-mode-abbrev-table 
 ("%"       . "<% $. %>")
 ("%%"      . "<%= $. %>")
 ("%for"    . "<% for $${elem} in $${list} %>$.<% end %>$>")
 ("%h"      . "<%=h $${@item} %>")
 ("%if"     . "<% if $${cond} -%>$.<% end -%>")
 ("%ifel"   . "<% if $${cond} -%>$.<% else -%><% end -%>")
 ("%unless" . "<% unless $${cond} -%>$.<% end -%>")
 ("%ff"      . "<%= form_for :$${item}, :action => \"$${update}\" %>$.<% end %>")
 ("%ft"      . "<%= form_tag :action => \"$${update}\" do %>$.<% end %>")
 ("%lia"     . "<%= link_to \"$${text}\", :action => \"$${index}\" %>")
 ("%liai"    . "<%= link_to \"$${text}\", :action => \"$${edit}\", :id => $${item} %>")
 ("%lic"     . "<%= link_to \"$${text}\", :controller => \"$${items}\" %>")
 ("%lica"    . "<%= link_to \"$${text}\", :controller => \"$${items}\", :action => \"$${index}\" %>")
 ("%licai"   . "<%= link_to \"$${text}\", :controller => \"$${items}\", :action => \"$${edit}\", :id => $${item} %>"))


(define-abbrev-table 'rails-controller-abbrev-table ())
(merge-abbrev-tables ruby-mode-abbrev-table rails-controller-abbrev-table)
(snippet-with-abbrev-table
 'rails-controller-abbrev-table
 ("rt"    . "render :text => '$${render}'")
 ("rf"    . "render :file => '$${filepath}'")
 ("ri"    . "render :inline => '$${hello}'")
 ("rl"    . "render :layout => '$${layoutname}'")
 ("rn"    . "render :nothing => $${true}")
 ("rp"    . "render :partial => '$${item}'")
 ("ra"    . "render :action => '$${index}'")
 ("re"    . "redirect_to :action => '$${index}'")
 ("rec"   . "redirect_to :controller => '$${items}'"))

(define-abbrev-table 'rails-model-abbrev-table ())
(merge-abbrev-tables ruby-mode-abbrev-table rails-model-abbrev-table)
(snippet-with-abbrev-table
 'rails-model-abbrev-table
 ("va"    . "validates_associated :$${attribute}")
 ("vc"    . "validates_confirmation_of :$${attribute}")
 ("ve"    . "validates_exclusion_of :$${attribute}")
 ("vu"    . "validates_uniqueness_of :$${attribute}")
 ("vp"    . "validates_presence_of :$${attribute}")
 ("vl"    . "validates_length_of :$${attribute}, :within => $${20}")
 ("bt"    . "belongs_to :$${model}")
 ("hm"    . "has_many :$${objects}")
 ("ho"    . "has_one :$${object}"))

(define-abbrev-table 'rails-migration-abbrev-table ())
(merge-abbrev-tables ruby-mode-abbrev-table rails-migration-abbrev-table)
(snippet-with-abbrev-table
 'rails-migration-abbrev-table
 ("tcls" . "t.column :$${title}, :$${string}\n$>tcls$.")
 ("tcl"  . "t.column :$${title}, :$${string}$.")
 ("tcln" . "t.column :$${title}, :$${string}, :null => false$.")
 ("acl"  . "add_column :$${,rails-snippets-feature:migration-table-name}, :$${column}, :$${string}")
 ("ai"   . "add_index :$${,rails-snippets-feature:migration-table-name}, $${column}")
 ("aiu"  . "add_index :$${,rails-snippets-feature:migration-table-name}, $${column}, :unique => true")
 ("rmcl" . "remove_column :$${,rails-snippets-feature:migration-table-name}, :$${column}")
 ("recl" . "rename_column :$${column}, :$${new_column}")
 ("dt"   . "drop_table :$${,rails-snippets-feature:migration-table-name}$.")
 ("ct"   . "create_table :$${,rails-snippets-feature:migration-table-name} do |t|\n$>tcls$.\nend$>")
 ("ret"  . "rename_table :$${,rails-snippets-feature:migration-table-name}, :$${new_name}$."))

