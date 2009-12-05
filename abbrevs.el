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
 ("h1"		    . "<h1>$.</h1>")
 ("h2"		    . "<h2>$.</h2>")
 ("h3"		    . "<h3>$.</h3>")
 ("h4"		    . "<h3>$.</h4>")
 ("h5"		    . "<h3>$.</h5>")
 ("h6"		    . "<h6>$.</h6>")
 ("div"	    	    . "<div>$.</div>")
 ("span"	    . "<span>$.</span>")
 ("form"	    . "<form action=\"$${action}\" method=\"$${post}\">$.</form>")
 ("input"	    . "<input type=\"$${text}\" name=\"$${name}\" value=\"$${value}\"/>")
 ("a"		    . "<a href=\"$${href}\">$.</a>")
 ("br"		    . "<br/>$.")
 ("ul"		    . "<ul>$.</ul>")
 ("ol"		    . "<ul>$.</ul>")
 ("li"		    . "<li>$.</li>")
 ("tab"	    	    . "<table>$.</table>")
 ("tr"		    . "<tr>$.</tr>")
 ("td"		    . "<td>$.</td>")
 ("th"		    . "<th>$.</th>")
 ("str"	    	    . "<strong>$.</strong>")
 ("em"		    . "<em>$.</em>")
 ("meta"	    . "<meta name=\"$${name}\" content=\"$${content}\"/>")
 ("style"	    . "<style type=\"text/css\">$.</style>")
 ("script"	    . "<script type=\"text/javascript\">$.</script>")
 ("img"	    	    . "<img src=\"$.\"/>")
 ("link"	    . "<link href=\"$${href}\" media=\"screen\" rel=\"stylesheet\" type=\"text/css\"/>"))

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
 ("rps"   . "render :partial => '$${item}', :status => $${500}")
 ("rt"    . "render :text => '$${render}'")
 ("rtl"   . "render :text => '$${render}', :layout => '$${layoutname}'")
 ("rtlt"  . "render :text => '$${render}', :layout => $${true}")
 ("rts"   . "render :text => '$${render}', :status => $${401}")
 ("rf"    . "render :file => '$${filepath}'")
 ("rfu"   . "render :file => '$${filepath}', :use_full_path => $${false}" )
 ("ri"    . "render :inline => '$${hello}'")
 ("ril"   . "render :inline => '$${hello}', :locals => { $${name} => '$${value}'$${4} }")
 ("rit"   . "render :inline => '$${hello}', :type => $${rxml}")
 ("rl"    . "render :layout => '$${layoutname}'")
 ("rn"    . "render :nothing => $${true}")
 ("rns"   . "render :nothing => $${true}, :status => $${401}")
 ("rp"    . "render :partial => '$${item}'")
 ("rpc"   . "render :partial => '$${item}', :collection => $${items}")
 ("rpl"   . "render :partial => '$${item}', :locals => { :$${name} => '$${value}'$${4} }")
 ("rpo"   . "render :partial => '$${item}', :object => $${object}")
 ("rcea"  . "render_component :action => '$${index}'")
 ("rcec"  . "render_component :controller => '$${items}'")
 ("rceca" . "render_component :controller => '$${items}', :action => '$${index}'")
 ("ra"    . "render :action => '$${index}'")
 ("ral"   . "render :action => '$${index}', :layout => '{default}'")
 ("rea"   . "redirect_to :action => '$${index}'")
 ("reai"  . "redirect_to :action => '$${show}', :id => $${item}")
 ("rec"   . "redirect_to :controller => '$${items}'")
 ("reca"  . "redirect_to :controller => '$${items}', :action => '$${list}'")
 ("recai" . "redirect_to :controller => '$${items}', :action => '$${show}', :id => $${item}"))

(define-abbrev-table 'rails-model-abbrev-table ())
(merge-abbrev-tables ruby-mode-abbrev-table rails-model-abbrev-table)
(snippet-with-abbrev-table
 'rails-model-abbrev-table
 ("va"    . "validates_associated :$${attribute}")
 ("vc"    . "validates_confirmation_of :$${attribute}")
 ("ve"    . "validates_exclusion_of :$${attribute}")
 ("vu"    . "validates_uniqueness_of :$${attribute}")
 ("vpif"  . "validates_presence_of :$${attribute}, :if => proc { |obj| $${condition} }")
 ("vp"    . "validates_presence_of :$${attribute}")
 ("vl"    . "validates_length_of :$${attribute}, :within => $${20}")
 ("bt"    . "belongs_to :$${model}")
 ("hm"    . "has_many :$${objects}")
 ("hmt"   . "has_many :$${objects}, :through => :$${,rails-snippets-feature:prev-has-many-table-name}")
 ("ho"    . "has_one :$${object}")
 ("habtm" . "has_and_belongs_to_many :$${object}"))

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


(define-abbrev-table 'ruby-test-abbrev-table ())
(merge-abbrev-tables ruby-mode-abbrev-table ruby-test-abbrev-table)
(snippet-with-abbrev-table
 'ruby-test-abbrev-table
 ("as"    . "assert $${test}")
 ("asa"   . "assert assigns(:$${variable})")
 ("ase"   . "assert_equal $${expected}, $${actual}")
 ("asid"  . "assert_in_delta $${expected_float}, $${actual_float}, $${20}")
 ("asio"  . "assert_instance_of $${ExpectedClass}, $${actual_instance}")
 ("asko"  . "assert_kind_of $${ExpectedKind}, $${actual_instance}")
 ("asm"   . "assert_match(/$${expected_pattern}/, $${actual_string})")
 ("asn"   . "assert_nil $${instance}")
 ("asne"  . "assert_not_equal $${unexpected}, $${actual}")
 ("asnm"  . "assert_no_match(/$${unexpected_pattern}/, $${actual_string})")
 ("asnn"  . "assert_not_nil $${instance}")
 ("asnr"  . "assert_nothing_raised $${Exception}  { $. }")
 ("asns"  . "assert_not_same $${unexpected}, $${actual}")
 ("asnt"  . "assert_nothing_thrown { $. }")
 ("aso"   . "assert_operator $${left}, :$${operator}, $${right}")
 ("asr"   . "assert_raise $${Exception} { $. }")
 ("asre"  . "assert_response :$${success}")
 ("asrt"  . "assert_respond_to $${object}, :$${method}")
 ("ass"   . "assert_same $${expected}, $${actual}")
 ("assd"  . "assert_send [$${object}, :$${message}, $${args}]")
 ("ast"   . "assert_throws :$${expected} { $. }")
 ("astm"  . "assert_template '$${index}'"))
