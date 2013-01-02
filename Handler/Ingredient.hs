module Handler.Ingredient
  (
    getIngredientR,
    postIngredientR
    )
where

import Import

entryForm :: Form Ingredient
entryForm = renderDivs $ Ingredient
    <$> areq   textField "Name" Nothing
    <*> areq   intField "Amount" Nothing

getIngredientR :: Handler RepHtml
getIngredientR = do
    -- Get the list of articles inside the database.
    ingredients <- runDB $ selectList [] [Desc IngredientName]
    -- We'll need the two "objects": articleWidget and enctype
    -- to construct the form (see templates/articles.hamlet).
    (ingredientWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        $(widgetFile "ingredients")

postIngredientR :: Handler RepHtml
postIngredientR = do
    ((res,_),_) <- runFormPost entryForm
    case res of
         FormSuccess ingredient -> do
            _ <- runDB $ insert ingredient
            setMessage $ toHtml $ (ingredientName ingredient) <> " created"
            redirect $ IngredientR
         _ -> do
            redirect $ IngredientR
