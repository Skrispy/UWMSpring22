fun max [] = null
| max ((i1,i2)::rest) = if i1 < i2 
                    then i1::max(rest) 
                    else i2::max(rest);